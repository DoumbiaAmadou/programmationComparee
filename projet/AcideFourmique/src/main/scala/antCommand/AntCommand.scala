package antCommand

/** Une AntCommande est une commande associee a une fourmi.
  * 
  * La construction d'une AntCommand peut etre facilitee par l'intermediaire
  * de l'operateur << fourni par la class [[Command]]
  */
final class AntCommand(ant: Int, command: Command) {
  override def toString: String = ant + ":" + command
}

/** Represente une commande de fourmi
  * 
  * Une commande peut etre [[Left]], [[Right]], [[Forward]], [[Rest]],
  * [[Attack]] ou [[Hack]].
  * Fournit un operateur de construction d'une [[AntCommand]].
  */
sealed abstract class Command {
  /** Operateur infixe qui construit une [[AntCommand]] */
  def <<(ant: Int) = new AntCommand(ant, this)
}

case class Left() extends Command { override def toString: String = "left" }
case class Right() extends Command { override def toString: String = "right" }
case class Forward() extends Command { override def toString: String = "forward" }
case class Rest() extends Command { override def toString: String = "rest" }

case class Attack(acidLevel: Int) extends Command {
  override def toString: String = "attack@" + acidLevel
}

case class Hack(code: Code) extends Command {
  override def toString: String = "hack@[" + code + ";]"
}
