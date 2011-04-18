package sefs


package object effect {
	implicit val ioMonad = IOMonad
	implicit val aioMonad = AIOMonad
	
	implicit def io2aio[A,S](io: IO[A]): AIO[A,S] = AIOMonad.convert(io)
	
	implicit def wrapAsIO[A](a: A): IO[A] = IOMonad pure a
}