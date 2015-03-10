package ants

import ants.behaviors._;

/* TODO: Peut etre remplacer direction [partie Comm qui decide]*/
abstract class Ant (  val numAnt: Int, 
                      val energy:Int, 
                      val acid:Int,
                      val direction: Int,
                      val behavior: Behavior){
  
  /* 
   * Pas le droit de modifier l'objet [Contrainte] donc on en créer un nouveau
   * à chaque opération
   */
  private[ants] def addEnergy(value: Int) : Ant
  private[ants] def addAcid(value: Int) : Ant
  private[ants] def changeBehavior(behavior: Behavior) : Ant
  
  def gainEnergy(value: Int) : Ant = {
    if(value > 0)
      addEnergy(value)
    else
      this
  }
  
  def looseEnergy(value: Int) : Ant = {
    if(value < 0)
      addEnergy(value)
    else
      this
  }
  
  def looseAcid(value: Int) : Ant = {
    if(value < 0)
      addAcid(value)
    else
      this
  }
  
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
  
  
  override def addEnergy(value: Int) = {
    new Ant(numAnt, energy+value, direction, acid, behavior) with Controlled
  }
  
  override def addAcid(value: Int) = {
    new Ant(numAnt, energy, acid+value, direction, behavior) with Controlled
  }
  
  override def changeBehavior(newBehavior: Behavior) = {
    new Ant(numAnt, energy, acid, direction, newBehavior) with Controlled
  }
  
  /** 
   * Une fourmie controllée peut attaquer
   * Le retour est de la forme d'un tuple : 
   * (Fourmie_Attaquant, Fourmie_Attaquée) 
   */
  def attack(other: Ant, acidAmount: Int) : Tuple2[Ant, Ant] = {
    val attackAcid = math.max(acid, acidAmount)
    val attackerAnt = this looseAcid attackAcid
    val attackedAnt = other looseEnergy attackAcid
    
    (attackerAnt, attackedAnt)
  }
  
  override def isControlled() = true
  
}


trait NotControlled extends Ant {
  
  override def addEnergy(value: Int) = {
    new Ant(numAnt, energy+value, acid, direction, behavior) with NotControlled
  }
  
  override def addAcid(value: Int) = {
    new Ant(numAnt, energy, acid+value, direction, behavior) with NotControlled
  }
  
  override def changeBehavior(newBehavior: Behavior) = {
    new Ant(numAnt, energy, acid, direction, newBehavior) with NotControlled
  }
  
  override def isControlled() = false
}