package ants

import ants.behaviors._
import comm.parse._;
import comm._;
import world.WorldMap

abstract class Ant (val numAnt: Int, val behavior: Behavior) {
  
  def isControlled() = false
    
  override def equals(o:Any) : Boolean = {
    o match {
      case a : Ant => if (this.numAnt == a.numAnt) true else false
      case _ => false
    }
  }
  
}

/* Decorateurs pour les fourmies en jeu */
trait Controlled extends Ant {
  
  override def isControlled() = true
  
  def playTurn(world: WorldMap) : SimpleCommand = {
    this.behavior live (world)
  }
  
}

/* Zombies et Enemies */
abstract trait NotControlled extends Ant {
    
  override def isControlled() = false
  
}

trait Enemy extends NotControlled
trait Zombie extends NotControlled

object AntFactory {
  
  def make(antState: AntState, behavior: Behavior) : Ant = {
    antState.brain match {
      case _ : Controlled => 
        new Ant(antState.id, behavior) with Controlled
        
      case _ : Enemy =>
        new Ant(antState.id, Independent) with Enemy
        
      case _ : Zombie =>
        new Ant(antState.id, Independent) with Zombie
    }
  }
  
}