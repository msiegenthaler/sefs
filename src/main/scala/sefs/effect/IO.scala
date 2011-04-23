package sefs
package effect

import scalaz.Monad

sealed trait IO[T] {
  private[effect] def perform: T

  //To resolve ambiguity
  def flatMap[B](f: T => IO[B]) = IOMonad.bind(this, f)
  def flatMap[B,S <: AIOExecution](f: T => AIO[B,S]) = AIOMonad.bind(AIOMonad.fromIO[T,S](this), f)
  def map[B](f: T => B) = flatMap(f andThen (x => IOMonad.pure(x)))
}

object IOMonad extends Monad[IO] {
  override def pure[A](a: => A): IO[A] = new IO[A] {
    override def perform = a
  }
  override def bind[A, B](a: IO[A], f: A => IO[B]): IO[B] = new IO[B] {
    override def perform = f(a.perform).perform
  }
}

/** UNSAFE: implement in libraries with side-effects (i.e. console, networking) */
trait IOImplementor {
  /** allowed to have side-effects */
  protected def io[T](f: => T): IO[T] = IOMonad.pure(f)
}

/** UNSAFE: only implement in special libraries */
trait IOPerformer {
  protected def perform[A](io: IO[A]): A = io.perform
}
