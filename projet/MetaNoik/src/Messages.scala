import akka.actor

(* server interactions *)
case class Auth(login: String, pass: String)
case class Create(users: String,
		  teaser: String,
		  pace: int,
		  nb_turn: int,
		  nb_ant: int,
		  nb_player: int,
		  min: int,
		  energy: int,
		  acid: int)
case class Destroy(id: String)
case class Join(id: String)
case class Logout()

(* ant actions *)
case class Left(id: int, etat: Etat)
case class Right(id: int, etat: Etat)
case class Forward(id: int, etat: Etat)
case class Rest(id: int, etat: Etat)
case class Attack(id: int, lvl: int, etat: Etat)
case class Hack(id: int, code: String, etat: Etat)
