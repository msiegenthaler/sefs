package sefs
package process

import effect._
import scalaz._
import Scalaz._
import scalaz.concurrent._

sealed trait Process {
  def !(msg: Any): IO[Unit]
}

sealed trait PS {
  private[process] val process: ProcessInternal
}
private object MsgBox extends MessageBoxContainer[Any]
import MsgBox._
private trait ProcessInternal extends Process {
  val msgbox: MessageBox
}

object Process extends AIOPerformer[PS] with AIOImplementor[PS] with IOImplementor {
  def spawn[A](body: AIO[A, PS])(implicit executor: Strategy): IO[Process] = io {
    val p = new ProcessImpl(executor)
    p.before
    perform(body, p.initial, (_: A, _) => p.after)
    p
  }

  def receive[A](pf: PartialFunction[Any, A]) = aio { (s: PS, cont: (A, PS) => Unit) =>
    val c = pf andThen (a => cont(a, s))
    s.process.msgbox setCapture c
  }
  //TODO receive within and receive no wait

  def self = aio { (s: PS, cont: (Process, PS) => Unit) =>
    cont(s.process, s)
  }

  //TODO do we need that?
  val noop = AIOMonad.pure[Unit, PS](())

  def processesRunning: IO[Boolean] = io(running.get == 0)

  def waitUntilAllProcessesTerminate: IO[Unit] = io {
  	while (running.get > 0) Thread.sleep(100)
  }

  private val running = new java.util.concurrent.atomic.AtomicLong
  private val pidDealer = new java.util.concurrent.atomic.AtomicLong
  private class ProcessImpl(exec: Strategy) extends ProcessInternal {
    val pid = pidDealer.incrementAndGet
    override val msgbox = new MessageBox(exec)
    override def !(msg: Any) = io(msgbox enqueue msg)
    def initial = new PS { override val process = ProcessImpl.this }
    def before: Unit = running.incrementAndGet
    def after: Unit = running.decrementAndGet
    override def toString = "<" + pid + ">"
  }
}
