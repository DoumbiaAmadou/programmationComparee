package world

import scala.collection.immutable.HashMap
import comm.parse._

abstract class  WorldMap(){
  
  /** @return : retourne le contenu de la case x,y*/
  def get(x:Int,y:Int) : (Option[Kind], Option[AntState])
  
  /** @return : retourne la liste des fourmis*/
  def ants: List[AntState]
}

case class WorldMapH (val cells : HashMap[(Int, Int), Kind], val ants : List[AntState]) extends WorldMap(){
  
  override def get(x:Int,y:Int) = {
    
    val kind = cells get((x,y))
    
    kind match {
      case Some(k) =>
        val ant = ants.find { a => x == a.x && y == a.y }
        (kind, ant)
      case None => (None, None)
    }
    
  }
  
  //override def ants: List[AntState] = ants_list//XXX à retirer si pas utile
  
}
