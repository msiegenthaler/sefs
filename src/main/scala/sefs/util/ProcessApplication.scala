package sefs
package util

import effect.IO
import process._
import Process._
import java.util.concurrent._

trait ProcessApplication extends IOApplication {
  protected val cpus = Runtime.getRuntime.availableProcessors
  protected val pool = Executors.newFixedThreadPool(cpus, new ThreadFactory {
    private val counter = new atomic.AtomicInteger
    override def newThread(r: Runnable) = {
      val t = new Thread(r, "processExecutor-" + counter.incrementAndGet)
      t.setDaemon(true)
      t
    }
  })
  protected implicit def executor: ProcessExecutor = new ProcessExecutor {
    override def apply(f: => Unit) = pool.submit(new Runnable { override def run = f })
  }

  override protected final def body = {
    for {
      p <- spawn(mainProcess)
      _ <- waitUntilAllProcessesTerminate
    } yield ()
  }

  protected def mainProcess: PIO[Unit]
}
