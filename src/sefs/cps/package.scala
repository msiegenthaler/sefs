package sefs

import effect.{ IO, IOMonad }
import scala.util.continuations._

package object cps {

  class IOCps[A] private[cps] (v: IO[A]) {
    def value: A @cps[IO[Any]] = shift(c => IOMonad.bind(v, c) )
    def exec = value
  }
  
  implicit def io2cps[A](a: IO[A]) = new IOCps(a)

  /** Converts a CPS-IO to a regular IO */
  def asIO[A](cps: => A @cps[IO[Any]]): IO[A] = {
    val ctx = reify[A, IO[Any], IO[Any]] {
      cps
    }
    val r = ctx.foreach(x => IOMonad.pure(x))
    r.asInstanceOf[IO[A]]
  }

}