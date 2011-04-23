package sefs
package cps

import scala.util.continuations._
import scalaz._

trait SpecificCps[M[_]] {
  protected val monad: Monad[M]

  class SpecificMonadCps[A] protected[cps] (v: M[A]) {
    def value: A @cps[M[Any]] = shift(c => monad.bind(v, c))
    def exec = value
  }

  implicit def sm2cps[A](a: M[A]) = new SpecificMonadCps[A](a)

  def asMonad[A](cps: => A @cps[M[Any]]): M[A] = {
    val ctx = reify[A, M[Any], M[Any]] { cps }
    val r = ctx.foreach(x => monad pure x)
    r.asInstanceOf[M[A]]
  }
}