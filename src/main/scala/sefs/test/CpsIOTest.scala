package sefs
package test

import effect.IOPerformer
import cps._
import io._
import effect._
import util._

object CpsIOTest extends IOApplication {
  override def body =  asMonad {
    ConsoleIO.println("One").exec
    ConsoleIO.println("Two").exec
    if (1 > 2)
      ConsoleIO.println("False").exec
    else
      ConsoleIO.println("True").exec
  }
}
