package world

import comm.parse.Observations
import scala.collection.immutable.HashMap
import comm.parse.Kind
import comm.parse.AntInfos
import comm.parse.AntView
import comm.parse.Grass

/*
 * On pourra créer une nouvelle valeur de world map à partir de l'ancienne
 * ainsi que des nouvelles observations faites par les fourmis
 */

object WorldMapCreator {

  def freshMap() : WorldMap = new WorldMap(new HashMap[(Int, Int), Kind], Nil)
  
  
  def make(oldMap : WorldMap, observation : Observations) : WorldMap = {
    
    val antStates = observation.ants_infos.map(x => x.ant_infos)
    
    new WorldMap(oldMap.cells ++ observeToCells(observation), antStates)
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