import akka.actor._

case class MapSquareConcrete(x: Int, y: Int) extends MapSquare

class MapUpdater(ia: ActorRef) extends Actor with ActorLogging {
	def handleMape(map: WorldMap, antmap: WorldMap, pos_ant: Pos) {
		antmap.squares foreach (_ foreach (_ match {
			case MapSquareConcrete(x, y) => map.squares(x + pos_ant.x)(y + pos_ant.y) 
							= antmap.squares(x)(y)
		}))
	}

	def handleObs(map: WorldMap, obs: List[Pair[AntState, WorldMap]]) = {
		obs foreach { 
			case (as, wm) => handleMape(map, wm, as.pos)
		}
	}

	def receive = {
		case Compute(obs, stat) => {
			handleObs(stat.map, obs)
			ia ! Map(obs, stat)
		}
	}
}
