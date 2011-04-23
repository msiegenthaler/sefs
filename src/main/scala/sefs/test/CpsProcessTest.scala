package sefs
package test

import effect._
import process._
import util.ProcessApplication
import sefs.cps.ProcessCps._
import sefs.io.ConsoleIO._
import sefs.io.Delay._

object CpsProcessTest extends ProcessApplication {

  override def mainProcess = asProcess {
    println("Start").exec
    val c = spawn {
      val c = self
      println("Child start " + c).exec
      val a = receive {
        case a: String => {
          println("Got " + a).exec
          a
        }
      }
      val b = receive {
        case a: String => {
          println("Got " + a).exec
          a
        }
      }
      println("Child end with " + a + ", " + b).exec
    }
    (c ! "Hi").value
    delay[PS](2000).exec
    (c ! "Ho").value
    println("End").exec
  }
}
