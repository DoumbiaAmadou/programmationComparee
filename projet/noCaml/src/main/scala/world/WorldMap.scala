package world

import scala.collection.immutable.HashMap
import comm.parse._

/** Carte du monde enregistrant le contenu de chaque case vues et la position
 *  de chaque fourmis
 */
abstract class  WorldMap(){
  
  /** @return : retourne le contenu de la case x,y*/
  def get(x:Int,y:Int) : (Option[Kind], Option[AntState])
  
  /** @return : retourne la liste des fourmis*/
  def ants: List[AntState]
}

/** Implémentation utilisant un HashMap
 *  
 *  @constructor créer une carte avec les cases connues et les fourmis en jeu
 *  @param cells un HashMap associant des coordonnées au type de terrain
 *  @param ants une liste de fourmis 
 */
case class WorldMapH (val cells : HashMap[(Int, Int), Kind],
                      val ants : List[AntState]) extends WorldMap () {
  
  override def get(x:Int,y:Int) = {
    
    val kind = cells get((x,y))
    
    kind match {
      /* Cas 1 : La case est connue, on recherche ensuite si une fourmis se 
       * trouve sur la case 
       */
      case Some(k) =>
        val ant = ants.find { a => x == a.x && y == a.y }
        (kind, ant)
      
      /* Cas 2 : La case n'a jamais été vue par nos fourmis, on n'a pas besoin
       * de chercher si une fourmis se trouve sur cette case car l'information
       * ne nous serait pas connue
       */
      case None => (None, None)
    }
    
  }
  
}
