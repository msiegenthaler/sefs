package sefs
package test

import cps.TraversableCps._
import util._
import scalaz._
import scala.math._

object ListCpsTest {

  val l1 = List(1, 2, 3)
  val l2 = List(1, 2, 3)
  val l3 = List(Some(1), None, Some(10))

  def main(args: Array[String]) = {
    val lp1 = asMonad {
      val la = (l1.values + 10) * 2
      (la, "asString=" + la)
    }
    println(lp1)

    val lp2 = asMonad {
      val b = (1 to 3)
      l2.values * (round(pow(10, b.values)))
    }
    println(lp2)

    val lp3 = asMonad {
      val operations = l3.values
      l2.values * operations.value
    }
    println(lp3)
  }

}
