package antCommand

/** Un code est une liste d'instructions associees a un label ou non.
  * 
  * Un Code va permettre d'injecter ses instructions a une fourmi morte
  * en utilisant la commande [[Hack]] d'une fourmi.
  */
final class Code(labelled_instrs: List[ (Option[Label], Instruction) ]) {
  override def toString: String = labelled_instrs map {
    case (Some(label), i) => label + ">" + i
    case (None, i) => i.toString
  } reduce ((i1,i2) => i1 + ";" + i2)
}

/** Represente un label pouvant etre associe a une [[Instruction]]
  * dans un [[Code]].
  * 
  * On peut facilement construire un Option[Label] en passant
  * par l'objet compagnon de cette classe.
  */
class Label(label: String) { override def toString: String = label }

/** Permet de construire un Option[Label].
  * 
  * ex: val Some(label) = Label("monLabel")
  */
object Label {
  /** Retourne un Option[Label]. */
  def apply(l: String) = Some(new Label(l))
}

/** Represente une variable
  * 
  * Fournit l'operateur := , qui facilite la
  * creation d'un Store.
  */
case class Var(identifier: String) {

  def :=(expr: Expression) = Store(this, expr)

  override def toString: String = identifier
}

/** Les instructions que peuvent executer les fourmis zombies */
abstract class Instruction

case class Store(variable: Var, expr: Expression) extends Instruction {
  override def toString: String = "store!" + variable + "!" + expr
}
case class Jump(label: Label) extends Instruction {
  override def toString: String = "jump!" + label
}
case class Jumpifz(variable: Var, label: Label) extends Instruction {
  override def toString: String = "jumpifz!" + variable + "!" + label
}
case class Fork() extends Instruction { override def toString: String = "fork" }


/** Definition d'une expression */
sealed abstract class Expression

case class EInt(n: Int) extends Expression {
  override def toString: String = n.toString
}
case class ValueOf(variable: Var) extends Expression {
  override def toString: String = "?" + variable
}
case class Add() extends Expression { override def toString: String = "add" }
case class Mul() extends Expression { override def toString: String = "mul" }
case class Div() extends Expression { override def toString: String = "div" }
case class Sub() extends Expression { override def toString: String = "sub" }
case class See() extends Expression { override def toString: String = "see" }
case class SeeAnt() extends Expression {
  override def toString: String = "see_ant"
}
/** Represente une application (e1 ... eN).
  * Un objet Apply se construit en lui passant
  * un nombre variable d'expression */ 
case class Apply(exprs: Expression*) extends Expression {
  override def toString: String =
    "(" + (exprs map (_.toString) reduce ((x,y) => x + " " + y)) + ")"
}
