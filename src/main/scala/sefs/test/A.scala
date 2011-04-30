package sefs
package test

import annotation._
import io.ConsoleIO._
import effect._

object AppA {
  println("One") //warn 1

  def main(arg: Array[String]): IO[Unit] = {
    println("Two") //warn 2
    readln //warn 3
    val a = doSome flatMap ((_: Unit) => doSome2("Three"))
    doSome //warn 4
    doSome2("Four") //warn 5 (last)
    println("Five")
  }

  def doSome: IO[Unit] = println("Six")
  def doSome2(v: String) = {
    val i = 1
    println(v)
  }
}
