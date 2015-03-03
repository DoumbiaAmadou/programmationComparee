package immutable_test

import scala.collection.immutable.Queue

class Word_immutable {
  

}

case class Immutable_Simple[T](values:T){
  var t = List[T](values)
  def get = t.head
  def set(value:T) = t = value::t
  override def toString = get toString()
}

object TestImmutable{
  def main(args: Array[String])={
    val x = 1
    val imm = Immutable_Simple(x)
    imm.set(2)
    println("imm="+imm)
    val y = 2
    imm.set(6)
    println("imm ="+imm)
  }
}