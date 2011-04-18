package sefs
package io

import effect._


object ConsoleIO extends IOImplementor {
  def println(text: String): IO[Unit] = io(Predef.println(text))
  def readln: IO[String] = io(Predef.readLine)
}
