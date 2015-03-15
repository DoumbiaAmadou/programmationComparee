package ants.ia

import ants.Ant
import world.WorldMap

//TODO: Pas sûr que ça reste
abstract class Strategy {

  def nextMove(ant : Ant, world : WorldMap)
  
}