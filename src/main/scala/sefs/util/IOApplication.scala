package sefs
package util

import effect._

trait IOApplication extends IOPerformer {
  def main(args: Array[String]): Unit = {
    val b = body
    println("*** Start ***")
    perform(body)
    println("***  End  ***")
  }
	
  protected def body: IO[Unit]
}
