package sefs
package effect

import scalaz._
import Scalaz._

sealed trait AIO[T, S] {
  private[effect] def perform(s: S, contFun: (T, S) => Unit)

  def flatMap[B](f: T => AIO[B, S]) = AIOMonad.bind(this, f)
  def map[B](f: T => B) = flatMap(f andThen (x => AIOMonad.pure[B, S](x)))
}

object AIOMonad {
  def pure[A, S](a: => A): AIO[A, S] = new AIO[A, S] {
    override def perform(s: S, contFun: (A, S) => Unit) = contFun(a, s)
  }
  def bind[A, B, S](a: AIO[A, S], f: A => AIO[B, S]): AIO[B, S] = new AIO[B, S] {
    override def perform(s: S, contFun: (B, S) => Unit) = {
      a.perform(s, (va, s2: S) => {
        val b = f(va)
        b.perform(s, contFun)
      })
    }
  }

  def convert[A, S](io: IO[A]): AIO[A, S] = new AIO[A, S] {
    override def perform(s: S, contFun: (A, S) => Unit) = {
      val a = io.perform
      contFun(a, s)
    }
  }
}

/**
 * UNSAFE: only implement in special libraries.
 * Prefer IOImplementor if possible (io -> aio conversion is always possible)
 */
trait AIOImplementor[S] {
  protected def aio[A](f: (S, (A, S) => Unit) => Unit): AIO[A, S] = new AIO[A, S] {
    override def perform(s: S, contFun: (A, S) => Unit) = f(s, contFun)
  }
  protected def aio[A](f: => A) = AIOMonad pure f
}

/** UNSAFE: only implement in special libraries */
trait AIOPerformer[S] {
  protected def perform[A](aio: AIO[A, S], initial: S, last: (A, S) => Unit = (x: A, s: S) => ()) =
    aio.perform(initial, last)
}