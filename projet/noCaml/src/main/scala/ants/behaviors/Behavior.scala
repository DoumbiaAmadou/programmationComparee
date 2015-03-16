package ants.behaviors

import world.WorldMap
import comm._
import scala.util.Random

/* L'IA des fourmies pourra utiliser un "behavior" pour determiner
 * sa prochaine action.
 */
abstract class Behavior () {
  
  def live(world : WorldMap) : SimpleCommand
  
}

/* Exemple de comportement possible (A approfondir) */

//Comportement des fourmis qu'on ne controle pas
object Independent extends Behavior {
  
  def live(world : WorldMap) : SimpleCommand = {
    throw new Exception("Independent.live : Not allowed")
  }
  
}

object FoodSeeker extends Behavior {

  def live(world : WorldMap) : SimpleCommand = {
    Left //TODO: Pour le moment, rien du tout a part tourner sur soi même
  }
  
}

object RandomExplorer extends Behavior {
  
  def live(world : WorldMap) : SimpleCommand = {
    val commands = List (Left, Right, Forward)
    commands(Random.nextInt(commands.length))
  }
  
}

object Forwarder extends Behavior {
  
  def live(world : WorldMap) : SimpleCommand = {
    Forward
  }
  
}