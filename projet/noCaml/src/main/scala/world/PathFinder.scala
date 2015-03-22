package world

import comm._
import comm.parse._

/*
 * TODO
 * outil utilisé pour trouver son chemin sur la map
 */
abstract class PathFinder {
  
  /*
   * essaye de trouver le meilleur chemin vers la position / le plus proche Kind element.
   * peut passer par les zones inconnues
   * évite l'eau et les rochers (sauf si Kind = eau/rocher)
   */
  def findPathTo(map: WorldMap, x:Int, y:Int, destX:Int, destY: Int) : SimpleCommand
  def findPathTo(map: WorldMap, x:Int, y:Int, element: Kind) : SimpleCommand
  
  /*
   * essaye de trouver le meilleur chemin vers la position / le plus proche Kind element.
   * évite les zones inconnues
   * évite l'eau et les rochers (sauf si Kind = eau/rocher)
   */
  def getClosestPathTo(map: WorldMap, x:Int, y:Int, destX:Int, destY: Int) : SimpleCommand
  def getClosestPathTo(map: WorldMap, x:Int, y:Int, element: Kind) : SimpleCommand
  
  /*
   * explore la zone inconnue la plus proche
   * évite l'eau et les rochers (sauf si Kind = eau/rocher)
   */
  def explore(map: WorldMap, x:Int, y:Int) : SimpleCommand
}