package sefs
package io

import effect._
import java.util.concurrent._

object Delay extends AIOImplementor {
  private val threadFactory = new ThreadFactory {
    override def newThread(r: Runnable) = {
      val t = new Thread(r, "Delayer")
      t setDaemon true
      t
    }
  }
  private val executor = new ScheduledThreadPoolExecutor(5, threadFactory)

  //TODO rewrite to use Duration
  def delay[S <: AIOExecution](ms: Long) = aio[Unit, S] { f =>
    val r = new Runnable { override def run = f(()) }
    executor.schedule(r, ms, TimeUnit.MILLISECONDS)
  }
}

