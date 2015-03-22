package ants

import ants.behaviors._
import comm.parse._;
import comm._;
import world.WorldMap

/** Une fourmis de jeu
 *  
 *  @constructor crée une fourmis numerotée avec un comportement
 *  @param numAnt l'id de fourmis donné par le serveur
 *  @param behavior le comportement que la fourmis adorptera 
 */
abstract class Ant (val numAnt: Int, val behavior: Behavior) {
  
  def isControlled() = false
    
  override def equals(o:Any) : Boolean = {
    o match {
      case a : Ant => if (this.numAnt == a.numAnt) true else false
      case _ => false
    }
  }
  
}

/** Représente les fourmis étant controllées par l'IA */
trait Controlled extends Ant {
  
  override def isControlled() = true
  
  /** Joue un tour selon le comportement à adopter
   * 
   * @param world la carte du tour actuel
   */
  def playTurn(world: WorldMap) : SimpleCommand = {
    this.behavior live (world)
  }
  
}

/** Représente les fourmis n'étant pas controllées par l'IA */ 
abstract trait NotControlled extends Ant {
    
  override def isControlled() = false
  
}

/** Cas particulier de fourmis non controllées */
trait Enemy extends NotControlled
trait Zombie extends NotControlled

/** Factory pour les instances de [ants.Ant] */
object AntFactory {
  
  /** Retourne une fourmis ayant un comportement
   * 
   * @param antState les informations du serveur parsé
   * @param behavior le comportement que la fourmis devra adopter
   * @return une fourmis avec le bon décorateur
   */
  def make(antState: AntState, behavior: Behavior) : Ant = {
    antState.brain match {
      case Controlled => 
        new Ant(antState.id, behavior) with Controlled
        
      case Ennemy =>
        new Ant(antState.id, Independent) with Enemy
        
      case Zombie =>
        new Ant(antState.id, Independent) with Zombie
    }
  }
  
}