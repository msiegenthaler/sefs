package test

//import scalaz._
//import Scalaz._

object B extends Application {

  type Message = Any

  trait Actor
  case class ActorWorld[A]()

  case class ActorST[S, A](f: ActorWorld[S] => (ActorWorld[S], A)) {
    def apply(s: ActorWorld[S]) = f(s)
    def map[B](g: A => B): ActorST[S, B] = ActorST(s => {
      val (s2, a) = f(s)
      (s2, g(a))
    })
    def flatMap[B](g: A => ActorST[S, B]): ActorST[S, B] = ActorST(s => {
      val (s2, a) = f(s)
      g(a)(s2)
    })
  }

  def returnActorST[S, A](a: => A): ActorST[S, A] = ActorST(s => (s, a))

  def send[S](to: Actor, msg: Message): ActorST[S, Unit] = returnActorST {
    println("Sending: " + msg)
    //TODO send it
  }

  val a1 = new Actor {}

  trait RealWorld
  def spawn[A](body: ActorST[RealWorld, A]) = {
  	val realWorld = ActorWorld[RealWorld]()
    body.apply(realWorld)
  }

  type S = RealWorld
  val x = for {
    s1 <- send[S](a1, "Hi") //TODO get rid of [S}
    s2 <- send[S](a1, "Ho")
  } yield s2
  println(x)

  val b = newActorBody[Unit]
  def newActorBody[S] = returnActorST[S, Unit](())

  println("Start")
  spawn(x)
  println("End")
  
  val y = x.flatMap(x => send(a1, "Bla"))
  spawn(y)

}