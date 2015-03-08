package ants

import ants.behaviors._

/* Permettra de former des groupes de fourmis qui auront un même
 * objectif en jeu
 * XXX: Système de formation (position) des fourmis ?
 */
class Group(val ants: List[Ant], groupBehavior: Behavior) {

  def addAnt(ant: Ant with Controlled) : Group = {
    new Group(ant::ants, groupBehavior)
  }
  
  def removeAnt(ant: Ant with Controlled) : Group = {
    if (ants contains ant)
      new Group(ants filter(x => (!(x equals ant))), groupBehavior)
    else
      this
  }
}