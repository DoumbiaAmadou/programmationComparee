package comm

sealed abstract class Command(val name:String){
  def parsed():String//correspond à la commande parsé pour le serveur
}
sealed abstract class SimpleCommand(override val name:String) extends Command(name){
  override def parsed()=name
}
case object Left extends SimpleCommand("left")
case object Right extends SimpleCommand("right")
case object Forward extends SimpleCommand("forward")
case object Rest extends SimpleCommand("rest")
case class  Atk(val n_level:Int) extends Command("attack"){
  override def parsed()=name+"@"+n_level
}
case class  Hack[subTypeCom <: Command](val code:subTypeCom)extends Command("hack"){
  override def parsed()={
    name+"["+code.parsed+"]"
  }
}

/*********Code - des fourmies zombies********/
sealed abstract class Code(override val name:String) extends Command(name)

sealed class CodeList[subTypeCom <: Command](codes:List[subTypeCom]) extends Code("--code list : error_parsing--"){
  override def parsed()=
    codes.length match{
      case 1=> codes.foldLeft("")((acc,c)=>acc+c.parsed)
      case _=> codes.foldLeft("")((acc,c)=>acc+c.parsed+";")
    }
}

case class Store(val variable:String,val expression:Expression) extends Code("store"){
  override def parsed() = name+"!"+variable+"!"+expression.parse
}
case class Jump(val label:String) extends Code("jump"){
  override def parsed() = name+"!"+label
}
case class Jumpifz(val variable:String,val label:String) extends Code("jumpifzi"){
  override def parsed() = name+"!"+variable+"!"+label
}
case object Fork extends Code("fork"){
  override def parsed() = name
}

/******Expressions ******/

sealed abstract class Expression{
  def parse():String
}
case class N(x:Int)extends Expression{
  override def parse()=x.toString 
}
case class Find(variable:String)extends Expression{
  override def parse()="?"+variable
}

/****Primitive*****/
sealed abstract class Primitive() extends Expression{
  override def parse():String
}
case class Add(a:Int,b:Int){
  override def parse()=a.toString +","+b.toString//XXX question prof qu'est ce qui sépare les arguments ???
}