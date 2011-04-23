package sefs
package test

import effect._
import process._
import Process._
import sefs.io.ConsoleIO._
import sefs.io.Delay._
import util._

object ProcessTest extends ProcessApplication {
  override protected def mainProcess = {
    val x: PIO[Any] = for {
      _ <- println("Welcome from main on thread " + Thread.currentThread.getName)
      p <- spawn {
        for {
          s <- self
          _ <- println("Hi from child " + s + " on thread " + Thread.currentThread.getName)
          m <- receive {
            case s: String => v(s)
          }
          _ <- println("Child " + s + " got message " + m)
          _ <- delay(3000)
          _ <- println("Bye from " + s + " on thread " + Thread.currentThread.getName)
        } yield ()
      }
      _ <- println("Spawned " + p)
      _ <- p ! "Hi"
      p2 <- spawn {
        for {
          s <- self
          _ <- println("Hi from child " + s + " on thread " + Thread.currentThread.getName)
          _ <- delay(1000)
          _ <- println("Bye from " + s + " on thread " + Thread.currentThread.getName)
        } yield ()
      }
      _ <- println("Spawned " + p2)
      _ <- println("Main done")
      _ <- noop
    } yield ().asInstanceOf[Any]
    x
  }
}
