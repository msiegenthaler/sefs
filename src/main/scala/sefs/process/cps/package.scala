//package sefs
//package processcps
//
//import effect._
//import sefs.cps._
//import scala.util.continuations._
//import sefs.process._
//import Process._
//
////package 
//object ProcessWithCps extends SpecificCps[AIO] {
//  protected val monad = AIOMonad
//
//  implicit def pio2cps[A](a: IO[A]) = sm2cps(io2aio(a))
//
//  def asProcess[A](cps: => A @cps[AIO[Any]]) = asMonad(cps)
//
//  def self = Process.self value
//  def spawn[A](body: AIO[A]) = Process.spawn(body) value
//}
