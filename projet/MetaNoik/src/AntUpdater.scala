import akka.actor._

class AntUpdater(ia: ActorRef) extends Actor with ActorLogging {
	def handleObs(map: WorldMap, obs: List[Pair[AntState, WorldMap]]) = {
		map
	}

	def receive = {
		case Compute(obs, stat) => {
			handleObs(stat.map, obs)
			ia ! Ants(obs, stat)
		}
	}
}
