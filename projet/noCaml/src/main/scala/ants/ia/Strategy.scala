package ants.ia

import ants.Ant

abstract class Strategy {

  def nextMove(ant : Ant, world : World)
  
}