package sefs
package test

import effect._
import process._
import util.ProcessApplication
import sefs.process.cps._
import sefs.io.ConsoleIO._
import sefs.io.Delay._

object CpsProcessTest extends ProcessApplication {

  override def mainProcess = main map (x => "".asInstanceOf[Any])

  def main: PIO[Unit] = asProcess {
    println("Hi").value
    val mainProcess = self
    println("Main process is " + mainProcess + " on thread " + Thread.currentThread.getName).value
    val c = spawn(child)
    println("Spawned child: " + c).value
    println("Bye").value
  }

  def child: PIO[Unit] = asProcess {
    val c = self
    println("Child " + c + " on thread " + Thread.currentThread.getName).value
    println("Delaying..... ").value
    delay[PS](2000).value
    println("Bye from child").value
  }

  import process._
  import Process._
  def main2: PIO[Unit] = for {
    _ <- println("Hi")
    m <- self
    _ <- println("Main process' is " + m + " on thread " + Thread.currentThread.getName)
    c <- spawn(child2)
    _ <- c ! "Hi"
    _ <- println("Spawned child " + c)
    _ <- println("Bye")
  } yield ()
  def child2: PIO[Unit] = for {
    c <- self
    _ <- println("Child' " + c + " on thread " + Thread.currentThread.getName)
    _ <- println("Delaying...")
    _ <- delay(2000)
    _ <- println("Bye from child")
  } yield ()

}
