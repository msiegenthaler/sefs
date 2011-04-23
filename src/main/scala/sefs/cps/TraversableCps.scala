package sefs
package cps

import scalaz._

object TraversableCps extends SpecificCps[Traversable] {
  protected val monad: Monad[Traversable] = implicitly
  implicit def t2cps[A](a: Traversable[A]) = sm2cps(a)
  implicit def option2cps[A](a: Option[A]) = sm2cps(option2Traversable(a))

  private def option2Traversable[A](o: Option[A]): Traversable[A] = o.map(List(_)).getOrElse(Nil)
}
