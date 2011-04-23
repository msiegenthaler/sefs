package sefs

import effect._
import scalaz.concurrent.Strategy

package object process extends KindOfAIO[PS] {
  type PIO[T] = KAIO[T]
  implicit val PIOMonad = KAIOMonad
  implicit def io2pio[A](io: IO[A]): PIO[A] = io2aio(io)
  def v[A](v: A): PIO[A] = KAIOMonad.pure(v)

  private[process] trait PIOImplementor extends KAIOImplementor
  private[process] trait PIOPerformer extends KAIOPerformer
}
