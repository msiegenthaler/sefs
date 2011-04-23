package sefs
package process

import effect._
import scalaz._
import Scalaz._

sealed trait PS extends AIOExecution {
  private[process] val process: ProcessInternal
}
object PS {
  type AIO_PS[T] = AIO[T,PS]
}

sealed trait Process {
  def !(msg: Any): IO[Unit]
}

trait ProcessExecutor {
  def apply(f: => Unit): Unit
}

private object MsgBox extends MessageBoxContainer[Any]
import MsgBox._
private trait ProcessInternal extends Process {
  val msgbox: MessageBox
}

object Process extends `package`.PIOPerformer with `package`.PIOImplementor with IOImplementor {
  def spawn[A](body: PIO[A])(implicit executor: ProcessExecutor): IO[Process] = io {
    val p = new ProcessImpl(executor)
    running.incrementAndGet
    val after = (_: A, _: PS) => {
      running.decrementAndGet
      ()
    }
    executor {
      perform(body, p.initial, after)
    }
    p
  }

  def receive[A](pf: PartialFunction[Any, PIO[A]]) = aio { (s: PS, cont: (A, PS) => Unit) =>
    val c = pf andThen { a => perform(a, s, cont) }
    s.process.msgbox setCapture c
  }
  //TODO receive within and receive no wait

  def self = aio { (s: PS, cont: (Process, PS) => Unit) =>
    cont(s.process, s)
  }

  val noop = PIOMonad.pure(())

  def processesRunning: IO[Boolean] = io(running.get == 0)

  def waitUntilAllProcessesTerminate: IO[Unit] = io {
    while (running.get > 0) Thread.sleep(333)
  }

  private val running = new java.util.concurrent.atomic.AtomicLong
  private val pidDealer = new java.util.concurrent.atomic.AtomicLong
  private class ProcessImpl(exec: ProcessExecutor) extends ProcessInternal {
    val pid = pidDealer.incrementAndGet
    override val msgbox = new MessageBox(exec)
    override def !(msg: Any) = io(msgbox enqueue msg)
    def initial = new PS {
      override val process = ProcessImpl.this
      override def execute(f: => Unit) = exec(f)
    }
    override def toString = "<" + pid + ">"
  }
}
