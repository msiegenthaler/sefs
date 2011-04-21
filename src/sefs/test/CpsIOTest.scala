package sefs
package test

import effect.IOPerformer
import cps._
import io._
import effect._

object CpsIOTest extends App with IOPerformer {
  val o: IO[String] = asMonad {
    ConsoleIO.println("One").exec
    ConsoleIO.println("Two").exec
    if (1 > 2)
      ConsoleIO.println("False").exec
    else
      ConsoleIO.println("True").exec
    "Three"
  }

  println("****")
  val result = perform(o)
  println("Result=" + result)
  println("Upper Result=" + result.toUpperCase)
  println("****")
}