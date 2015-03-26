package comm

import aliases.Type._

/** Ce fichier contient toutes les commandes que peuvent faire les fourmis.
 *  
 *  Note : Il n'y a que Parse.play qui reçoit une liste de commande. 
 */


sealed abstract class Command(val name:String){
  def parsed():String//correspond à la commande parsé pour le serveur
}

/***** Les commandes de Base *******/
sealed abstract class SimpleCommand(override val name:String)
                      extends Command(name){
  override def parsed()=name
}
case object Left extends SimpleCommand("left")
case object Right extends SimpleCommand("right")
case object Forward extends SimpleCommand("forward")
case object Rest extends SimpleCommand("rest")
case class  Atk(val n_level:Int) extends Command("attack"){
  override def parsed()=name+"@"+n_level
}

case class Hack[SubCom <: Command](val inst_list:List[(OptionLabel,SubCom)])
           extends Command("hack"){
  override def parsed()="["+content+"]"
    
  private def content()=
    inst_list.foldLeft("")(  (acc,i)=>acc
                               +label_match(i._1) 
                               +i._2.parsed + ";"
                          )

  private def label_match(label:OptionLabel)=
    label match{
      case None =>""
      case Some(x) => x+">"
    }
}
  
/*********Instruction(Code) - des fourmies zombies********/
sealed abstract class Instruction(override val name:String) extends Command(name)

case class Store[SubEx <: Expression](val variable:String,val expression:SubEx) extends Instruction("store"){
  override def parsed() = name+"!"+variable+"!"+expression.parsed
}
case class Jump(val label:String) extends Instruction("jump"){
  override def parsed() = name+"!"+label
}
case class Jumpifz(val variable:String,val label:String) extends Instruction("jumpifzi"){
  override def parsed() = name+"!"+variable+"!"+label
}
case object Fork extends Instruction("fork"){
  override def parsed() = name
}

/****Expressions****/

sealed abstract class Expression(){
  def parsed():String
}
case class N(x:Int)extends Expression{
  override def parsed()=x.toString 
}
case class Find(variable:String)extends Expression{
  override def parsed()="?"+variable
}
//case class Eval(l:List[Expression])extends Expression//XXX manque de spec

/****Primitive****/
sealed abstract class Primitive(val name:String) extends Expression{
  override def parsed()=name
}
sealed abstract class BinPrim(name:String,a:Int,b:Int)extends Primitive(name){
  override def parsed()=name+"("+a.toString +","+b.toString+")"//XXX manque de spec
}
case class Add(a:Int,b:Int)extends BinPrim("add",a,b)
case class Sub(a:Int,b:Int)extends BinPrim("sub",a,b)
case class Mul(a:Int,b:Int)extends BinPrim("mul",a,b)
case class Div(a:Int,b:Int)extends BinPrim("div",a,b)

case object See extends Primitive("see")
case object SeeAnt extends Primitive("see_ant")