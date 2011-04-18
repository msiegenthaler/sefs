package sefs
package util

import effect._

trait IOApplication extends IOPerformer {
	def main(args: Array[String]): Unit = {
		perform(body)
	}
	
	protected def body: IO[Unit]
}