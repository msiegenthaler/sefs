package sefs
package test

import effect._
import io.ConsoleIO._
import process._
import Process._
import util._

object ProcessTest extends ProcessApplication {
  override protected def mainProcess = {
    for {
      _ <- println("Welcome")
      p <- spawn {
        for {
          s <- self
          _ <- println("Hi from child " + s)
          m <- receive {
            case s: String => s
          }
          _ <- Thread.sleep(1000)
          _ <- println("Child " + s + " got message " + m)
          _ <- println("Byte from " + s)
        } yield ()
      }
      _ <- println("Spawned " + p)
      _ <- p ! "Hi"
      _ <- println("Main done")
      _ <- noop
    } yield ()
  }
}
