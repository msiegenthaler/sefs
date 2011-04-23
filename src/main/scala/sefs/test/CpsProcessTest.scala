package sefs
package test

import io.ConsoleIO._
import effect._
import process._
import util.ProcessApplication
import sefs.processcps.ProcessWithCps._
import scalaz._
import Scalaz._


object CpsProcessTest extends ProcessApplication {
  
  override def mainProcess = main map (x => "".asInstanceOf[Any])

  def main: AIO[Unit] = asProcess {
    println("Hi").value
    val mainProcess = self
    println("Main process is " + mainProcess).value
    val c = spawn(child)
    println("Spawned child: "+c).value
    println("Bye").value
  }


  def child: AIO[Unit] = asProcess {
    val c = self
    println("Child "+c).value
    wrapIO(Thread.sleep(1000)).value
    println("Bye from child").value
  }

  import Process._
  def main2: AIO[Unit] = for {
    _ <- println("Hi")
    m <- self
    _ <- println("Main process' is "+m+" on thread "+Thread.currentThread.getName)
    c <- spawn(child2)
    _ <- c ! "Hi"
    _ <- println("Spawned child "+c)
    _ <- println("Bye")
  } yield ()
  def child2: AIO[Unit] = for {
    c <- self
    _ <- println("Child' "+c+" on thread "+Thread.currentThread.getName)
    _ <- wrapIO(Thread.sleep(1000))
    _ <- println("Bye from child")
  } yield ()

}
