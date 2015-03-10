package map

class Position(val environment : Int = 0, val ant: String = "") {
  def makePosition (a: String) : Position = {
    var res : Position = new Position(environment,a); 
    return res;
  }  
}

object Position {
  val unknown: Int = 0;
  val grass: Int = 1;
  val water: Int = 2;
  val rock: Int = 3;
  val food: Int = 4;  
 // type ground = (unknown,grass,water,rock,food);
}