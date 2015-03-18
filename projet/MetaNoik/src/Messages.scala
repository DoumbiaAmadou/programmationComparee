import akka.actor

(* server Interactions *)
case class Auth(login: String, pass: String)
case class Create(users: String,
		  teaser: String,
		  pace: Int,
		  nb_turn: Int,
		  nb_ant: Int,
		  nb_player: Int,
		  min: Int,
		  energy: Int,
		  acid: Int)
case class Destroy(id: String)
case class Join(id: String)
case class Logout()

(* ant actions *)
case class Left(id: Int, etat: Etat)
case class Right(id: Int, etat: Etat)
case class Forward(id: Int, etat: Etat)
case class Rest(id: Int, etat: Etat)
case class Attack(id: Int, lvl: Int, etat: Etat)
case class Hack(id: Int, code: String, etat: Etat)
