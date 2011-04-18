package sefs

import effect._
import scalaz._
import Scalaz._
import scalaz.concurrent._

package object process {

  sealed trait Process {
    def !(msg: Any): IO[Unit] = Process.send(this)(msg)
  }

  sealed trait PIO[T] {
    private[process] def perform(process: ProcessInternal, continue: T => Unit)
  }

  private trait ProcessInternal extends Process {
    val msgbox: ProcessMessageBox.MessageBox
  }
  private object ProcessMessageBox extends MessageBoxContainer[Any]

  object Process extends IOImplementor with IOPerformer {
    private val pidDealer = new java.util.concurrent.atomic.AtomicLong

    //TODO exception handler
    def spawn[T](body: PIO[T])(implicit executor: Strategy): IO[Process] = io {
      val p = ProcessImpl(pidDealer.incrementAndGet, executor)
      executor {
        body.perform(p, _ => ())
      }
      p
    }

    def send(to: Process)(msg: Any): IO[Unit] = io {
      to match {
        case to: ProcessInternal => to.msgbox enqueue msg
        case _ => throw new AssertionError("Unsupported process type: " + to.getClass)
      }
    }

    import ProcessMonad._

    def self: PIO[Process] = pure { (p, cont) => cont(p) }

    def receive[A](pf: PartialFunction[Any, A]): PIO[A] = pure { (p, cont) =>
      val c = pf.andThen(a => cont(a))
      p.msgbox setCapture c
    }
    //TODO receive within (s)
    //TODO receive no wait

    implicit def pio[A](io: IO[A]): PIO[A] = ProcessMonad.pure { (p, cont) => cont(perform(io)) }

    private case class ProcessImpl(pid: Long, executor: Strategy) extends Process with ProcessInternal {
      val msgbox = new ProcessMessageBox.MessageBox(executor)
      override def toString = "<" + pid + ">"
    }
  }

  implicit object ProcessMonad extends Monad[PIO] {
    private[process] def pure[A](fun: (ProcessInternal, A => Unit) => Unit) = new PIO[A] {
      override def perform(process: ProcessInternal, continue: A => Unit) = fun(process, continue)
    }
    override def pure[A](a: => A): PIO[A] = new PIO[A] {
      override def perform(process: ProcessInternal, continue: A => Unit) = continue(a)
    }
    def bind[A, B](a: PIO[A], f: A => PIO[B]): PIO[B] = new PIO[B] {
      override def perform(process: ProcessInternal, continue: B => Unit) = {
        a.perform(process, { a =>
          val fb = f(a)
          fb.perform(process, continue)
        })
      }
    }
  }
}

object ProcessTest extends IOPerformer {
  import io.ConsoleIO._
  import process._
  import Process._

  implicit val executor = Strategy.Naive

  def main(args: Array[String]) = {
    perform {
      for {
        _ <- println("start of main")
        p <- spawn(for {
          s <- self
          _ <- pio(println("start of child " + s))
          m <- receive { case s: String => s }
          _ <- pio(println("Got message " + m))
          c <- pio(spawn {
            for {
              c <- self
              _ <- pio(println("Hi from " + c))
              m <- receive { case a: (Process, String) => a }
              _ <- m._1 ! "Got " + m._2
              x <- println("End of " + c)
            } yield x
          })
          _ <- pio(c ! (s, "Forward " + m))
          r <- receive { case s: String => s }
          _ <- println("Reply: " + r)
          x <- println("end of child " + s)
        } yield x)
        _ <- p ! "Hi"
        x <- println("end of main")
      } yield x
    }
  }
}

