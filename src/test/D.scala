package test

object D extends Application {
	
	case class Jid(id: String)
	sealed trait XmppPacket
	case class Message(receiver: Jid, msg: String) extends XmppPacket
	case class IQRequest(sender: Jid, receiver: Jid, message: String) extends XmppPacket
	case class IQResponse(sender: Jid, receiver: Jid, message: String) extends XmppPacket
	
	type XmppAgent = XmppPacket => Unit
	
	trait XmppAgentServices {
		val jid: Jid
		def send(packet: XmppPacket)
	}
	object XmppAgentServices {
		def jid(implicit agent: XmppAgentServices) = agent.jid
		def send(packet: XmppPacket)(implicit agent: XmppAgentServices) = agent send packet
	}
	
	
	import XmppAgentServices._
	
	implicit val services = new XmppAgentServices {
		override val jid = Jid("myself@my.com")
		override def send(packet: XmppPacket) = println("Sending "+packet)
	}
	
	val myAgent = (packet: XmppPacket) => packet match {
		case Message(from, msg) => 
			println("Got "+msg+" from "+from)
			send(Message(jid, "Echo "+msg))
		case _ => //ignore
	}
	
	object TypeSplittingAgent {
		def apply(iq: IQRequest => IQResponse = unsupportedIQ, message: Message => Unit = unsupportedMessage)
				(implicit agent: XmppAgentServices) = (packet: XmppPacket) => packet match {
			case m: Message => message(m)
			case r: IQRequest => send(iq(r))(agent)
			case other => //ignore
		}
		def unsupportedIQ(req: IQRequest) = IQResponse(req.receiver, req.sender, "Unknown")
		def unsupportedMessage(req: Message) = ()
	}
	
	val otherAgent = TypeSplittingAgent(
		message = (msg: Message) => println("Got "+msg.msg)
	)
	
	type IO[T] = T => Unit  
	
	sealed trait ConnectionState
	object Connected extends ConnectionState
	object Disconnected extends ConnectionState

	type AO = Either[XmppPacket,ConnectionState]
	
	trait XmppComponent {
		def processHandle(packet: XmppPacket): IO[Unit]
		def connectionState(state: ConnectionState): IO[Unit]
	}
	trait XmppComponentCallback {
		def send(packet: XmppPacket): IO[Unit]
	}
	
	object XmppServer {
		//todo how to disconnect?
		//todo side effect is very nasty
		//todo isn't this a bit hard to program against (output fun only after providing input fun)
		def connect(host: String, port: Int)(handler: XmppPacket => Unit): XmppPacket => Unit = {
			//blabla
			msg => println("Sending "+msg)
		}
	}

	
	
}