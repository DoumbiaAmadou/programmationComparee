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
  * Pour constuire un label, on passera par son objet compagnon.
  */
class Label private (label: String) { override def toString: String = label }
/** Permet de construire un label.
  * 
  * On construit un label en passant par cet objet et en lui fournissant
  * un nom de label de type String.
  * La construction renvoie un Option[Label].
  * ex: val Some(label) = Label("monLabel")
  */
object Label {
  /** Permet de construire un label.
    * Retourne un Option[Label].
    */
  def apply(l: String) = Some(new Label(l))
}

/** Les instructions que peuvent executer les fourmis zombies */
sealed abstract class Instruction /* TODO: Definir une instruction */

case class Fork() extends Instruction { override def toString: String = "fork" }
