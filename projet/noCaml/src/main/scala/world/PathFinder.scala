package world

import comm.SimpleCommand
import comm.parse.Kind

/*
 * TODO
 * tool used to find the good ways to go from a point to another
 */
class PathFinder {
  
  /*
   * try to find the closest searched element (position or Kind) traveling unknowns areas.
   * if element: kind isn't water, avoid it
   */
  def findPathTo(map: WorldMap, x:Int, y:Int, destX:Int, destY: Int) : SimpleCommand
  def findPathTo(map: WorldMap, x:Int, y:Int, element: Kind) : SimpleCommand
  
  /*
   * try to find the closest searched element (position or Kind) avoiding unknowns areas
   * if element: kind isn't water, avoid it
   */
  def getClosestPathTo(map: WorldMap, x:Int, y:Int, destX:Int, destY: Int) : SimpleCommand
  def getClosestPathTo(map: WorldMap, x:Int, y:Int, element: Kind) : SimpleCommand
  
  /*
   * go to the closest unknown area avoiding water
   */
  def explore(map: WorldMap, x:Int, y:Int) : SimpleCommand
}