package sefs
package effect

import scalaz._
import Scalaz._

trait AIOKind[S] {
  sealed trait AIO[T] {
    private[effect] def perform(s: S, contFun: (T, S) => Unit)
  }

  implicit object AIOMonad extends Monad[AIO] {
    override def pure[A](a: => A): AIO[A] = new AIO[A] {
      override def perform(s: S, contFun: (A, S) => Unit) = contFun(a, s)
    }
    override def bind[A, B](a: AIO[A], f: A => AIO[B]): AIO[B] = new AIO[B] {
      override def perform(s: S, contFun: (B, S) => Unit) = {
        a.perform(s, (va, s2: S) => {
          val b = f(va)
          b.perform(s, contFun)
        })
      }
    }
  }

  implicit def io2aio[A](io: IO[A]): AIO[A] = new AIO[A] {
    override def perform(s: S, contFun: (A, S) => Unit) = {
      val a = io.perform
      contFun(a, s)
    }
  }

  // helps mixed for comprehension
  implicit def ioWrapper[A](v: IO[A]): IOinAIO[A] = new IOinAIO(v)
  class IOinAIO[A](v: IO[A]) {
    def flatMap[B](f: A => IO[B]) = IOMonad.bind(v, f)
    def flatMap[B](f: A => AIO[B]) = AIOMonad.bind(io2aio(v), f) 
    def map[B](f: A => B) = flatMap(f andThen (x => IOMonad pure x))
  }

  /**
   * UNSAFE: only implement in special libraries.
   * Prefer IOImplementor if possible (io -> aio conversion is always possible)
   */
  trait AIOImplementor {
    protected def aio[A](f: (S, (A, S) => Unit) => Unit): AIO[A] = new AIO[A] {
      override def perform(s: S, contFun: (A, S) => Unit) = f(s, contFun)
    }
    protected def aio[A](f: => A) = AIOMonad pure f
  }

  /** UNSAFE: only implement in special libraries */
  trait AIOPerformer {
    protected def perform[A](aio: AIO[A], initial: S, last: (A, S) => Unit = (x: A, s: S) => ()) =
      aio.perform(initial, last)
  }
}