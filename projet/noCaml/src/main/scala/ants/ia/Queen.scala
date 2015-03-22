package ants.ia

import ants.Ant
import world.WorldMap
import comm.parse.AntState
import ants.AntFactory
import ants.behaviors.FoodSeeker
import ants.Controlled
import comm.Parse
import comm.parse.Observations
import ants.behaviors.RandomExplorer
import ants.behaviors.Forwarder
  
/** La reine de toutes les fourmis en jeu
 * 
 * @constructor créer une reine qui fera jouer les fourmis
 * @param parse une instance de [comm.parse.Parse] pour communiquer avec le serveur
 *        les actions de toutes les fourmis
 *        
 * @note La reine servira d'intermediaire entre [comm.parse.Parse] et [ants.Ant] pour
 *       récupérer les actions de chaque fourmis
 */
class Queen (val parse : Parse[_]) {
  
  /** Va faire jouer toutes les fourmies controllées 
   * 
   * @param world la carte du monde du tour actuel
   * @return transmet le retour de parse après avoir joué
   */
   def turn(world : WorldMap) : Observations = {
    val ants = this antsInit world
    
    /** Pour créer les commandes des fourmis, on va dans un premier temps
     *  ne garder que les fourmis controllées. Puis ensuite on les fera
     *  jouer
     */
    val commands = ants.filter {
      _ match {
        
        /** Cas 1 : c'est une fourmis controllée, on la garde */
        case ant : Ant with Controlled => true
        
        /** Cas 2 : c'est une fourmis non controléle, on ne la garde pas */
        case _ => false /** On ne garde pas*/
      }
    }.map {
      /** Pattern matching effectué pour récupérer le type Ant with Controlled */
      _ match {
        case ant : Ant with Controlled => (ant.numAnt, ant.playTurn(world))
        /** ce cas n'arrivera pas puisque la liste est filtrée juste avant */
        case _ => throw new Exception("Queen.turn exception")
      }
    }
    
    parse.play(commands)
  }

  /** Fonction auxilière faisant appel à [ans.AntFactory] pour créer les fourmis
   *  qui devront jouer
   */
  private def antsInit(world : WorldMap) : List[Ant] = {
    
    def aux(states : List[AntState]) : List[Ant] = {
      states match {
        case Nil => Nil
        case a::t => AntFactory.make(a, Forwarder):: aux(t) //TODO: Autres comportements
      }
    }
    aux(world.ants)
  }
  
}