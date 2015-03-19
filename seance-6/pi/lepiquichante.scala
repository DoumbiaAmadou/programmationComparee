import akka.actor._

case class ProduceOne
case class SubCall(a: List[ActorRef])
case class Reponse(k: Long, n: Long)

class Rand extends Actor with ActorLogging {
	val rand = scala.util.Random
	def receive = {
		case ProduceOne => {
			var k = 0
			var n = 0
			for (i <- 1 to 1000000) {
				val x = rand.nextDouble()
				val y = rand.nextDouble()
				n = n + 1
				if (x*x + y*y <= 1) { k = k + 1 }
			}
                        sender ! Reponse(k, n)
                }
        }
}

class Starter extends Actor with ActorLogging {
        var tot : Long = 0
        var sup : Long = 0

        def receive = {
                case SubCall(a) => a.foreach (_ ! ProduceOne)
                case Reponse(k, n) => {
                        tot = tot + 1
                        sup += k
			val tmp = (4 * sup * 1000000) / tot
                        log.info(tmp.toString)
			sender ! ProduceOne
                }
        }
}

object Hello {
        val system = ActorSystem("MySystem")
        val hello = system.actorOf(Props[Starter], name = "hello")
        var i = 0

        def main(arg: Array[String]) {
		hello ! SubCall((List.fill (1000) (1)).map (x => system.actorOf(Props[Rand], name = x.toString)))
        }
}
