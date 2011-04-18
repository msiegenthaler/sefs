package sefs

import scalaz._
import Scalaz._

package object effect {

  trait IOExecution

  sealed trait IO[T] {
    private[effect] def perform: T
  }

  implicit object IOMonad extends Monad[IO] {
    override def pure[A](a: => A): IO[A] = new IO[A] {
      override def perform = a
    }
    override def bind[A, B](a: IO[A], f: A => IO[B]): IO[B] = new IO[B] {
      override def perform = f(a.perform).perform
    }
  }

  trait IOImplementor {
    protected def io[T](f: => T): IO[T] = IOMonad.pure(f)
  }
  trait IOPerformer {
    protected def perform[A](io: IO[A]): A = {
      io.perform
    }
  }
}
