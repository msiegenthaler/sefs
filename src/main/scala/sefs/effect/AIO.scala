package sefs
package effect

import scalaz._

trait AIOExecution {
  private[effect] def execute(f: => Unit): Unit
}

sealed trait AIO[T, S <: AIOExecution] {
  private[effect] def perform(s: S, contFun: (T, S) => Unit)

  // to resolve ambiguity
  def flatMap[B](f: T => AIO[B, S]) = AIOMonad.bind(this, f)
  def map[B](f: T => B) = flatMap(f andThen (x => AIOMonad.pure[B, S](x)))
}

trait AIOImplementor {
  protected def aioValue[A, S <: AIOExecution](v: => A): AIO[A, S] = AIOMonad.pure[A, S](v)
  protected def aio[A, S <: AIOExecution](f: (A => Unit) => Unit) = new AIO[A, S] {
    override def perform(s: S, c: (A, S) => Unit) = {
      val c2: A => Unit = a => c(a, s)
      f(c2)
    }
  }
}

object AIOMonad {
  def pure[A, S <: AIOExecution](a: => A): AIO[A, S] = new AIO[A, S] {
    override def perform(s: S, contFun: (A, S) => Unit) = contFun(a, s)
  }
  def bind[A, B, S <: AIOExecution](a: AIO[A, S], f: A => AIO[B, S]): AIO[B, S] = new AIO[B, S] {
    override def perform(s: S, contFun: (B, S) => Unit) = {
      a.perform(s, (va, s2: S) => s2 execute {
        val b = f(va)
        b.perform(s2, contFun)
      })
    }
  }

  def fromIO[A, S <: AIOExecution](io: IO[A]): AIO[A, S] = new AIO[A, S] {
    override def perform(s: S, contFun: (A, S) => Unit) = {
      val a = io.perform
      contFun(a, s)
    }
  }

}

trait KindOfAIO[S <: AIOExecution] {
  protected type KAIO[T] = AIO[T, S]

  object KAIOMonad extends Monad[KAIO] {
    override def pure[A](a: => A) = AIOMonad pure a
    override def bind[A, B](a: KAIO[A], f: A => KAIO[B]) = AIOMonad.bind(a, f)
  }

  def io2aio[A](io: IO[A]): KAIO[A] = new KAIO[A] {
    override def perform(s: S, contFun: (A, S) => Unit) = {
      val a = io.perform
      contFun(a, s)
    }
  }

  /**
   * UNSAFE: only implement in special libraries.
   * Prefer IOImplementor if possible (io -> aio conversion is always possible)
   */
  protected trait KAIOImplementor {
    protected def aio[A](f: (S, (A, S) => Unit) => Unit): KAIO[A] = new KAIO[A] {
      override def perform(s: S, contFun: (A, S) => Unit) = f(s, contFun)
    }
    protected def aioValue[A](v: => A) = KAIOMonad pure v
  }

  /** UNSAFE: only implement in special libraries */
  protected trait KAIOPerformer {
    protected def perform[A](aio: KAIO[A], initial: S, last: (A, S) => Unit = (x: A, s: S) => ()) =
      aio.perform(initial, last)
  }
}
