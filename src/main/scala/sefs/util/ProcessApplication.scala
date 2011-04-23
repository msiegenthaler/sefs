package sefs
package util

import effect._
import process._
import Process._

trait ProcessApplication extends IOApplication {
  override protected final def body = {
    for {
      _ <- spawn(mainProcess)
      _ <- waitUntilAllProcessesTerminate
    } yield ()
  }

  protected implicit val executor = scalaz.concurrent.Strategy.DefaultExecutorService
  protected def mainProcess: AIO[Any]
}
