package ants.behaviors

import world.WorldMap
import comm._
import scala.util.Random

/** Représente le comportement qu'une fourmis pourra adopter en jeu
 * 
 *  L'IA des fourmies utilisera cette classe pour determiner
 *  sa prochaine action.
 *  
 *  La création de nouveaux comportements se fera en étendant cette classe
 */
abstract class Behavior () {
  
  /** retourne la commande représentant la prochaine action de la fourmis
   * 
   * @param world la carte du monde du tour actuel
   * @return une commande utilisable par [comm.parse.Parse]
   */
  def live(world : WorldMap) : SimpleCommand
  
}


/** Comportement des fourmis qu'on ne controle pas */
object Independent extends Behavior {
  
  def live(world : WorldMap) : SimpleCommand = {
    throw new Exception("Independent.live : Not allowed")
  }
  
}

/** Comportement des fourmis chercheuses de nourriture */
object FoodSeeker extends Behavior {

  /**
   * @note cette methode n'est pas implémentée correctement
   */
  def live(world : WorldMap) : SimpleCommand = {
    Left
  }
  
}

/** Comportement produisant un deplacement de fourmis aléatoire */
object RandomExplorer extends Behavior {
  
  /**
   * @note Les deplacements possibles sont [comm.Command.Left]
   *       [comm.Command.Right] et [comm.Command.Forward]
   */
  def live(world : WorldMap) : SimpleCommand = {
    val commands = List (Left, Right, Forward)
    commands(Random.nextInt(commands.length))
  }
  
}

/** Comportement faisant marcher les fourmis tout droit */
object Forwarder extends Behavior {
  
  def live(world : WorldMap) : SimpleCommand = {
    Forward
  }
  
}