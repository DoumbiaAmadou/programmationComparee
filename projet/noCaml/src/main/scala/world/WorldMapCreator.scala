package world

import comm.parse.Observations
import scala.collection.immutable.HashMap
import comm.parse._


/** Factory pour les instances de [world.WorldHMap]
 *  
 * @note une nouvelle valeur de world map se créer à partir de l'ancienne
 *       ainsi que des nouvelles [comm.Parse.Observations] faites par les fourmis
 */
object WorldMapHCreator {

  /** Retourne la nouvelle valeur de la carte du monde
   * 
   * @param oldMap la valeur de la map du tour précedent
   * @param observation les observations faites par nos fourmis
   * @return une nouvelle carte mise à jour avec les nouvelles observations
   */
  def make(oldMap : Option[WorldMap], observation : Option[Observations]) : WorldMap = 
    (oldMap,observation) match {
      case (None,None) =>new WorldMapH(new HashMap[(Int, Int), Kind], Nil)
      
      case (Some(wmh:WorldMapH),Some(obs)) =>
            val antStates = observation.get.ants_infos.map( _.ant_infos )
            new WorldMapH(wmh.cells ++ observeToCells(obs), antStates)
            
      case _ => throw new Exception("WorldMapHCreator.make exception")
    }

  /** Retourne les observations sous forme de HashMap pour mettre à jour la
   *  carte du monde du tour actuel
   */
  private def observeToCells(observation : Observations) : HashMap[(Int, Int),
                                                                    Kind] = {
    
    /** Fonction auxilière pour permettre de transformer un [comm.parse.AntView]
     *  en HashMap associant les coordonnées à un type de terrain
     */
    def createCells(antView : List[AntView], acc : HashMap[(Int, Int), Kind] ) 
          : HashMap[(Int, Int), Kind] = {
      
      antView match {
        case Nil => acc
        case a::t => 
          println(a.content)
          createCells(t, acc + ((a.x, a.y) -> a.content(0)))
      }
      
    }
    
    /** Fonction auxilière permettant de transformer une liste de [comm.parse.AntInfos]
     *  en HashMap en utilisant la fonction createCells
     */
    def createMap(antsInfo : List[AntInfos], acc : HashMap[(Int, Int), Kind]) 
          : HashMap[(Int, Int), Kind]= {
      
      antsInfo match {
        case Nil => acc
        case a::t => createMap(t, acc ++ createCells(a.ant_view, new HashMap))
      }
    }
    
    createMap(observation.ants_infos, new HashMap)
  }
  
  /** Test */
  def main(args: Array[String]) {
    
    val map = HashMap[Int,Int]() +  (1->2) + (2->3)
    
    println(map(1))
    println(map(5))
    
  }  
}