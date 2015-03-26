import akka.actor._

abstract class Command
/*
 * These are the commands that can be sent to the ant
 * each command has an identifier so that it can send a command
 * to a precise ant
 */

case class Left(id: Int) extends Command
case class Right(id: Int) extends Command
case class Forward(id: Int) extends Command
case class Rest(id: Int) extends Command
case class Attack(id: Int, power: Int) extends Command
case class Hack(id: Int, code: String) extends Command

abstract class Message

/* server Interactions */

/* IN */
case class Auth(login: String, pass: String) extends Message
case class Create(val users: String,
                  val teaser: String,
                  val pace: Int,
                  val nb_turn: Int,
                  val nb_ant: Int,
                  val nb_player: Int,
                  val min: Int,
                  val energy: Int,
                  val acid: Int) extends Message
case class Destroy(id: String) extends Message
case class Join(id: String) extends Message
case class Logout() extends Message
case class Play(id: Int, cmds: List[Pair[Int,Command]], status: Status) extends Message

/* OUT */
case class Observation(turn: Int, 
		       obs: List[Pair[AntState, WorldMap]], 
		       status: Status) extends Message

case class Compute(obs: List[Pair[AntState,WorldMap]], status: Status) extends Message

case class Ants(obs: List[Pair[AntState,WorldMap]], status: Status) extends Message
case class Map(obs: List[Pair[AntState,WorldMap]], status: Status) extends Message
