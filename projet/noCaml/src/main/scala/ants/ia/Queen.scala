package ants.ia

import ants.Ant
import world.WorldMap
import comm.parse.AntState
import ants.AntFactory
import ants.behaviors.FoodSeeker
import ants.Controlled
import comm.Parse
import comm.parse.Observations
import ants.behaviors.RandomExplorer
import ants.behaviors.Forwarder
  

class Queen (val parse : Parse[_])]) {
this(parse)super()
   ()
  
   def turn(world : WorldMap): Observations = { 
    val ants = this antsInit world
    
    val commands = ants.filter {
        _ match {
          case ant : Ant with Controlled => true
          case _ => false
        }
      }.map
      _ match {
        case ant : Ant with Controlled => (ant.numAnt, ant.playTurn(world))
        case _ => throw new Exception("Queen.turn exception")
      }
    }
    
    parse.play(commands)
  }

  private def antsInit(world : WorldMap): List[Ant] = {
    def aux(states : List[AntState]): List[Ant] = {
      states match {
        case Nil => Nil
        case a::t => AntFactory.make(a, Forwarde):: AntFactory.make(a, Forwarder):: aux(t) //TODO: Autres comportements
      }
    }
    aux(world.ants)
  }
  
}