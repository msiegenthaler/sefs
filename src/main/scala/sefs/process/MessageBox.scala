package sefs
package process

import scalaz.concurrent._
import java.util.concurrent.atomic._

/**
 * Message box for many senders and a single consumer.
 */
private trait MessageBoxContainer[T] {
  type Capture = PartialFunction[T, Unit]
  type Cancel = Capture => Unit

  private trait Captures {
    def add(capture: Capture): Captures
    def add(cancel: Cancel): Captures
    def apply(old: Option[Capture]): (Option[Capture], Option[Cancel])
    def resultsInCapture(current: Boolean): Boolean
  }
  private object Captures0 extends Captures {
    override def add(capture: Capture) = new Captures1A(capture)
    override def add(cancel: Cancel) = new Captures1R(cancel)
    override def apply(old: Option[Capture]) = (old, None)
    override def resultsInCapture(current: Boolean) = current
  }
  private class Captures1A(capture: Capture) extends Captures {
    override def add(captureNew: Capture) =
      throw new IllegalStateException("Already a capture registered (A)")
    override def add(cancel: Cancel) =
      new Captures2AR(capture, cancel)
    override def apply(old: Option[Capture]) = {
      if (old.isDefined)
        throw new IllegalStateException("Uncancelled capture (A)")
      (Some(capture), None)
    }
    override def resultsInCapture(current: Boolean) = true
  }
  private class Captures1R(cancel: Cancel) extends Captures {
    override def add(capture: Capture) =
      throw new IllegalStateException("Processing continued before cancel was executed")
    override def add(cancelNew: Cancel) =
      throw new IllegalStateException("Already a cancel registerd (R)")
    override def apply(old: Option[Capture]) = {
      old.foreach(cancel(_))
      (None, None)
    }
    override def resultsInCapture(current: Boolean) = false
  }
  private class Captures2AR(capture: Capture, cancel: Cancel) extends Captures {
    override def add(captureNew: Capture) = {
      cancel(capture)
      new Captures1A(captureNew)
    }
    override def add(cancelNew: Cancel) =
      throw new IllegalStateException("Already a cancel registerd (AR)")
    override def apply(old: Option[Capture]) = {
      if (old.isDefined)
        throw new IllegalStateException("Uncancelled capture (AR)")
      (Some(capture), Some(cancel))
    }
    override def resultsInCapture(current: Boolean) = false
  }

  private case class Actions(in: List[T], captures: Captures, checkerRunning: Boolean, hasMsgs: Boolean, hasCapture: Boolean)
  private object ActionsRMC extends Actions(Nil, Captures0, true, true, true)
  private object ActionsRfC extends Actions(Nil, Captures0, true, false, true)
  private object ActionsRMf extends Actions(Nil, Captures0, true, true, false)
  private object ActionsRff extends Actions(Nil, Captures0, true, false, false)
  private object ActionsfMC extends Actions(Nil, Captures0, false, true, true)
  private object ActionsffC extends Actions(Nil, Captures0, false, false, true)
  private object ActionsfMf extends Actions(Nil, Captures0, false, true, false)
  private object Actionsfff extends Actions(Nil, Captures0, false, false, false)

  private case class State(msgs: List[T], capture: Option[Capture])

  /**
   * Removes the first matching element from the list and returns it along with the
   * rest of the list. Returns None if no element matches.
   */
  private def removeFirst[A](list: List[A], filter: A => Boolean) = {
    def removeFirst_(head: List[A], tail: List[A]): Option[(A, List[A])] = tail match {
      case ele :: tail =>
        if (filter(ele)) Some(ele, head reverse_::: tail)
        else removeFirst_(ele :: head, tail)
      case Nil => None
    }
    removeFirst_(Nil, list)
  }
  
  
  /**
   * Message box for many senders and a single consumer.
   */
  class MessageBox(checkExec: Strategy) {
    //Actions to execute. Modified and read by all threads
    private val pending = new AtomicReference[Actions](Actions(Nil, Captures0, false, false, false))
    //State of the message box. Only accessed by the checker-threads (not-concurrent)
    @volatile
    private var state = State(Nil, None)

    /**
     * Enqueue a new message.
     * Does complete fast, does never block. The amount of code executed in the caller thread
     * is minimal. No capture check is done in this thread.
     */
    def enqueue(msg: T): Unit = {
      val actions = pending.get
      val msgs = msg :: actions.in
      if (actions.checkerRunning || !actions.hasCapture) {
        val na = actions.copy(in = msgs, hasMsgs = true)
        if (!pending.compareAndSet(actions, na)) enqueue(msg) // retry
      } else {
        val na = actions.copy(in = msgs, hasMsgs = true, checkerRunning = true)
        if (pending.compareAndSet(actions, na)) checkExec(check)
        else enqueue(msg) //retry
      }
    }

    /**
     * Register a new capture for the message box.
     */
    def setCapture(capture: Capture): Unit = {
      val actions = pending.get
      val ncs = actions.captures.add(capture)
      if (actions.checkerRunning || !actions.hasMsgs) {
        val na = actions.copy(captures = ncs, hasCapture = true)
        if (!pending.compareAndSet(actions, na)) setCapture(capture) //retry
      } else {
        val na = actions.copy(captures = ncs, hasCapture = true, checkerRunning = true)
        if (pending.compareAndSet(actions, na)) checkExec(check)
        else setCapture(capture) //retry
      }
    }
    /**
     * Cancel the currently registered capture.
     * The 'cancel' will be called with the deregistered capture. If no capture is registered
     * then this method is a no-op, 'cancel' will not be called.
     */
    def cancelCapture(cancel: Cancel): Unit = {
      val actions = pending.get
      val ncs = actions.captures.add(cancel)
      if (actions.checkerRunning || !actions.hasCapture) {
        val na = actions.copy(captures = ncs)
        if (!pending.compareAndSet(actions, na)) cancelCapture(cancel) //retry
      } else {
        val na = actions.copy(captures = ncs, checkerRunning = true)
        if (pending.compareAndSet(actions, na)) checkExec(check)
        else cancelCapture(cancel) //retry
      }
    }

    /**
     * Register a capture and cancel it right away. The capture will check all msgs received
     * so far and the be canceled.
     */
    def setPeekCapture(capture: Capture, cancel: Cancel): Unit = {
      val actions = pending.get
      val ncs = actions.captures.add(capture).add(cancel)
      if (actions.checkerRunning) { // must run always, else the capture does not get cancelled
        val na = actions.copy(captures = ncs, hasCapture = true)
        if (!pending.compareAndSet(actions, na)) setPeekCapture(capture, cancel) //retry
      } else {
        val na = actions.copy(captures = ncs, hasCapture = true, checkerRunning = true)
        if (pending.compareAndSet(actions, na)) checkExec(check)
        else setPeekCapture(capture, cancel) //retry
      }
    }

    /**
     * Guarantees:
     * - only active once ('synchronized') and only in the checkExec queue
     * - sees every msg without external delay
     */
    private def check = {
      def process(s: State): Unit = {
        val actions = pending.get

        val runningAction = {
          if (actions.in.nonEmpty || s.msgs.nonEmpty) {
            if (actions.captures.resultsInCapture(s.capture.isDefined)) ActionsRMC
            else ActionsRMf
          } else {
            if (actions.captures.resultsInCapture(s.capture.isDefined)) ActionsRfC
            else ActionsRff
          }
        }

        //Confirm that we received the action..
        if (!pending.compareAndSet(actions, runningAction)) process(s)
        else {
          val (capture, deferredCancel) = actions.captures(s.capture)
          //try to apply the capture and merge the action-msgs into the state
          val s2 = processMsgs(s, capture, deferredCancel, actions.in.reverse)

          // need to do that before cas, because everything after cas might overlap with next invocation
          state = s2

          val notRunningAction = {
            if (s2.msgs.nonEmpty) {
              if (s2.capture.isDefined) ActionsfMC
              else ActionsfMf
            } else {
              if (s2.capture.isDefined) ActionsffC
              else Actionsfff
            }
          }

          //See if we can terminate..
          if (!pending.compareAndSet(runningAction, notRunningAction)) {
            // new messages/captures arrived, so we need to rerun ourselves
            process(s2)
          }
        }
      }
      process(state)
    }

    /**
     * Process messages and apply the captures.
     * @param s current state
     * @param capture the capture to apply (new/ols capture)
     * @param actionMsgs new messages
     * @return new state
     */
    private def processMsgs(s: State, capture: Option[Capture], deferredCancel: Option[Cancel], actionMsgs: List[T]): State = capture match {
      case c@Some(capture) =>
        if (s.capture == capture) {
          //only check the new msgs
          removeFirst(actionMsgs, capture.isDefinedAt _) match {
            case Some((msg, rest)) =>
              capture(msg)
              State(s.msgs ::: rest, None)
            case None =>
              val msgs = s.msgs ::: actionMsgs
              deferredCancel match {
                case Some(cancel) =>
                  cancel(capture)
                  State(msgs, None)
                case None =>
                  State(msgs, c)
              }
          }
        } else {
          //check all msgs
          val msgs = if (!actionMsgs.isEmpty) s.msgs ::: actionMsgs else s.msgs
          removeFirst(msgs, capture.isDefinedAt _) match {
            case Some((msg, rest)) =>
              capture(msg)
              State(rest, None)
            case None =>
              deferredCancel match {
                case Some(cancel) =>
                  cancel(capture)
                  State(msgs, None)
                case None =>
                  State(msgs, c)
              }
          }
        }
      case None =>
        val msgs = if (!actionMsgs.isEmpty) s.msgs ::: actionMsgs else s.msgs
        State(msgs, None)
    }
  }
}