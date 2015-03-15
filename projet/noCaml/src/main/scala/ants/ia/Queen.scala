package ants.ia

import ants.Ant
import world.WorldMap
import comm.parse.AntState
import ants.AntFactory
import ants.behaviors.FoodSeeker
import ants.Controlled
  

class Queen () {
  
  def antsInit(world : WorldMap) : List[Ant] = {
    
    def aux(states : List[AntState]) : List[Ant] = {
      states match {
        case Nil => Nil
        case a::t => AntFactory.make(a, FoodSeeker):: aux(t) //TODO: Autres comportements
      }
    }
    
    aux(world.ants)
  }
  
  
  def turn(world : WorldMap) : Unit = { 
    
    val ants = this antsInit world
        
    val commands = ants.map {
      _ match {
        case ant : Ant with Controlled => ant.playTurn(world)
        case _ => ()
      }
    }.filter { _ != () }
    
  }
  
}