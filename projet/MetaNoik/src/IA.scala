import akka.actor._

class IA(mapup: ActorRef, antup: ActorRef, cynthia: ActorRef) extends Actor with ActorLogging {
	def receive = {
		case Observation(turn, obs, status) => {
			antup ! Compute(obs, status)
		}
		case Ants(obs, status) => {
			mapup ! Compute(obs, status)
		}
		case Map(obs, status) => {
			var cmds = List((1, Left(0)))
			cynthia ! Play(0, cmds, status)
		}
	}
}
