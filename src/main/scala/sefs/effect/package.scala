package sefs

package object effect {
  implicit val ioMonad = IOMonad
  implicit def wrapIO[A](a: A): IO[A] = IOMonad pure a
}
