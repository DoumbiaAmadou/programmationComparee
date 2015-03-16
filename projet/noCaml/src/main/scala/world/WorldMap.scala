package world

import scala.collection.immutable.HashMap
import comm.parse._

abstract class  WorldMap(){
  
  /** @return : retourne le contenu de la case x,y*/
  def get(x:Int,y:Int)
  
  /** @return : retourne la liste des fourmis*/
  def ants: List[AntState]
}

case class WorldMapH (val cells : HashMap[(Int, Int), Kind], val ants : List[AntState]) extends WorldMap(){
  
  override def get(x:Int,y:Int) = null//TODO
  
  //override def ants: List[AntState] = ants_list//XXX à retirer si pas utile
  
}
