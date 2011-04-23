package sefs
package test

import cps._
import util._
import scalaz._
import scala.math._

object ListCpsTest {

  val l1 = List(1, 2, 3)
  val l2: Traversable[Int] = List(1, 2, 3)

  def main(args: Array[String]) = {
    val lp1 = asMonad[List, (Int, String)] {
      val la = (l1.values + 10) * 2
      (la, "asString=" + la)
    }
    println(lp1)

    val lp2 = asMonad[Traversable, Any] {
      val b: Traversable[Int] = (1 to 3)
      l2.values * (round(pow(10, b.values)))
    }
    println(lp2)
  }

}
