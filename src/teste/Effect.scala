package teste

import scalaz._
import Scalaz._

//trait M[T] {
//	def flatMap[A](f: T => M[A])(implicit m: Monad[M[T]]) = 
//}
//
//trait Monad[M[_]] {
//  def unit[A](a: A): M[A]
//  def bind[A, B](m: M[A])(f: A => M[B]): M[B]
//}

sealed trait IO[T] {
  private[teste] def perform: T
}

object IOMonad extends Monad[IO] {
  override def pure[A](a: => A): IO[A] = new IO[A] {
    override def perform = a
  }
  def bind[A, B](a: IO[A], f: A => IO[B]): IO[B] = new IO[B] {
    override def perform = f(a.perform).perform
  }
}

trait IOImplementor {
  private trait IOImpl[T] extends IO[T]
  protected def io[T](f: => T): IO[T] = IOMonad.pure(f)
}
trait IOPerformer {
  protected def perform[A](io: IO[A]): A = io.perform
}

object ConsoleIO extends IOImplementor {
  def println(text: String): IO[Unit] = io(Predef.println(Thread.currentThread.getName + "> " + text))
  def readln: IO[String] = io(Predef.readLine)
}

trait Process {
  def !(msg: Any): IO[Unit] = Process.send(this)(msg)
}
object Process extends IOImplementor with IOPerformer {
  import scalaz.concurrent._

  def send(to: Process)(msg: Any): IO[Unit] = {
    //TODO
    ConsoleIO.println("Process " + to + " got message " + msg)
  }

  private val counter = new java.util.concurrent.atomic.AtomicLong
  def spawn[T](body: IO[T])(implicit executor: Strategy): IO[Process] = io {
    val sp = new Process {
      val pid = counter.incrementAndGet
      override def toString = "<" + pid + ">"
    }
    executor(perform(body))
    sp
  }
}

object X extends IOPerformer {
  import ConsoleIO._
  implicit val iom = IOMonad
  import Process._

  def main(arg: Array[String]) = {
    implicit val executor = scalaz.concurrent.Strategy.Naive

    val b = for {
      _ <- println("Hi from B")
      _ <- println("Welcome")
      _ <- println("Please enter command")
      in <- readln
      _ <- println("You entered " + in)
      x <- println("Bye from B")
    } yield x

    perform(for {
      _ <- println("Hi from A")
      p <- spawn(b)
      _ <- println("Spawned "+p)
      _ <- p ! "Hi"
      _ <- println("end of main thread")
      x <- println("Bye from A")
    } yield x)
  }
}

//trait Effect[T] {
//  //	def unit[A](v: A): Effect[A]
//  def flatten[A](x: Effect[Effect[A]]): Effect[A]
//  def map[A](fun: T => A): Effect[A] = flatMap(x => unit(fun(x)))
//  def flatMap[A](fun: T => Effect[A]): Effect[A] = flatten(map(fun))
//}
//object Effect {
//  def unit[A] = new Effect[A]
//
//}