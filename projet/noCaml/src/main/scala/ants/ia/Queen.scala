package ants.ia

import comm.Parse
import ants.Ant

//Pour le moment je mets ici jusqu'a avoir la vraie classe
//Les methodes sont fictives et pas representatives de la classe finale
class World {
  def closestFood(x:Int, y:Int) : Option[Tuple2[Int,Int]] = None
  def closestAnt(x:Int, y:Int) : Option[Tuple2[Int,Int]] = None
}
  

class Queen (parser: Parse) {

  
  def turn(world: World, ants: List[Ant]) : Unit = { //Un peu à l'aveugle
    
  }
  
}