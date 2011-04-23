package sefs
package cps

import effect._
import scala.util.continuations._
import process._
import Process._

object ProcessCps extends SpecificCps[PS.AIO_PS] {
  protected override val monad = PIOMonad

  implicit def io2cps[A](a: IO[A]) = sm2cps(io2pio(a))
  implicit def aio2cps[A](a: AIO[A,PS]) = sm2cps(a)

  def asProcess[A](cps: => A @cps[PIO[Any]]) = asMonad(cps)

  def self = Process.self value
  def spawn[A](body: PIO[A])(implicit executor: ProcessExecutor) = Process.spawn(body) value
}
