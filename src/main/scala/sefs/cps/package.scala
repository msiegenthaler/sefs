package sefs

import scala.util.continuations._
import scalaz._

package object cps {

  class MonadCps[M[_], A] protected[cps] (v: M[A]) {
    def value(implicit m: Monad[M]): A @cps[M[Any]] = shift { c: (A => M[Any]) =>
      m.bind(v, c)
    }
    def values(implicit m: Monad[M]) = value(m)
    def exec(implicit m: Monad[M]) = value(m)
  }

  implicit def m2cps[M[_], A](a: M[A]) = new MonadCps[M, A](a)

  def asMonad[M[_], A](cps: => A @cps[M[Any]])(implicit m: Monad[M]): M[A] = {
    val ctx = reify[A, M[Any], M[Any]] { cps }
    val r = ctx.foreach(x => m pure x)
    r.asInstanceOf[M[A]]
  }
}
