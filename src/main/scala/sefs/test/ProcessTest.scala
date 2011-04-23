package sefs
package test

import effect._
import io.ConsoleIO._
import process._
import Process._
import util._
import scalaz._
import Scalaz._

object ProcessTest extends ProcessApplication {

  override protected def mainProcess = {
    val x: AIO[Any] = for {
      _ <- println("Welcome from main on thread " + Thread.currentThread.getName)
      p <- spawn {
        for {
          s <- self
          _ <- println("Hi from child " + s + " on thread " + Thread.currentThread.getName)
          m <- receive {
            case s: String => s
          }
          _ <- wrapIO(Thread.sleep(1000))
          _ <- println("Child " + s + " got message " + m)
          _ <- println("Bye from " + s)
        } yield ()
      }
      _ <- println("Spawned " + p)
      _ <- p ! "Hi"
      _ <- println("Main done")
      _ <- noop
    } yield ().asInstanceOf[Any]
    x
  }
}
