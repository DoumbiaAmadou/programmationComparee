import akka.actor

abstract class Command

case class Left(id: Int)
case class Right(id: Int)
case class Forward(id: Int)
case class Rest(id: Int)
case class Attack(id: Int, power: Int)
case class Hack(id: Int, code: String)

abstract class Message

/* server Interactions */
/* IN */
case class Auth(login: String, pass: String) extends Message
case class Create(users: String,
		  teaser: String,
		  pace: Int,
		  nb_turn: Int,
		  nb_ant: Int,
		  nb_player: Int,
		  min: Int,
		  energy: Int,
		  acid: Int) extends Message
case class Destroy(id: String) extends Message
case class Join(id: String) extends Message
case class Logout() extends Message
case class Play(id: Int, cmds: List[Pair[Int,Command], status: Status) extends Message

/* OUT */
case class Observation(turn: Int, 
		       obs: List[Pair[AntState,Map]], 
		       status: Status) extends Message



case class Compute(obs: List[Pair[AntState,Map]], status: Status)

case class Ants(obs: List[Pair[AntState,Map]], status: Status)
case class Map(obs: List[Pair[AntState,Map]], status: Status)
