package test


object A extends Application {
	
	type Message = Any
	type Communication = (Actor,Message)
	val No: List[Communication] = Nil
	
	sealed trait Actor {
		def !(msg: Message) = {
			behaviour.lift(msg).getOrElse(enqueue(msg))
		}
		protected def enqueue(msg: Message): (Option[Actor],List[Communication]) = {
			//TODO
			(Option(this), No)
		}
		protected val behaviour: PartialFunction[Message,(Option[Actor],List[Communication])]
	}
	object Actor {
		def apply(f: PartialFunction[Message,(Option[Actor],List[Communication])]): Actor = {
			new Actor { 
				override protected val behaviour = f
			} 
		}
	}
	
	
	val a = Actor {
		case msg: String =>
			println("Got "+msg)
			(None, Nil)
			
	}

	a ! "Hi"
}