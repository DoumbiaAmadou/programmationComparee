package world

import comm.parse.Observations
import scala.collection.immutable.HashMap
import comm.parse._


/*
 * On pourra créer une nouvelle valeur de world map à partir de l'ancienne
 * ainsi que des nouvelles observations faites par les fourmis
 */

object WorldMapHCreator {

  def make(oldMap : Option[WorldMap], observation : Option[Observations]) : WorldMap = 
    (oldMap,observation) match{
      case (None,None) =>new WorldMapH(new HashMap[(Int, Int), Kind], Nil)
      
      case (Some(wmh:WorldMapH),Some(obs)) =>
            val antStates = observation.get.ants_infos.map(x => x.ant_infos)
            new WorldMapH(wmh.cells ++ observeToCells(obs), antStates)
            
      case _ => throw new Exception("WorldMapHCreator.make exception")
                null
    }

  
  private def observeToCells(observation : Observations) : HashMap[(Int, Int),
                                                                    Kind] = {
    
    def createCells(antView : List[AntView], acc : HashMap[(Int, Int), Kind] ) 
          : HashMap[(Int, Int), Kind] = {
      
      antView match {
        case Nil => acc
        case a::t => createCells(t, acc + ((a.x, a.y) -> a.content(0)))
      }
      
    }
    
    def createMap(antsInfo : List[AntInfos], acc : HashMap[(Int, Int), Kind]) 
          : HashMap[(Int, Int), Kind]= {
      
      antsInfo match {
        case Nil => acc
        case a::t => createMap(t, acc ++ createCells(a.ant_view, new HashMap))
      }
    }
    
    createMap(observation.ants_infos, new HashMap)
  }
      
}