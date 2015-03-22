package game

import aliases.Type._
import comm._
import comm.parse.Observations
import sys.process._
import world._
import ants.ia._
import ErrorCode._


/** Game est la classe qui initialise et lance la boucle principale.
 *  La classe Game contient tout les paramamètres nécéssaire au pour
 *  créer une partie.
 *  
 *  Note : Au moment de ce créer Game crée une instance de Parce.
 *         Game et Parce se référencent mutuellement. Le but est de fixer
 *         l'instance de Game et de Parse. Ceci empèche une fois créer de
 *         modifier l'état interne de Game et donc de faire des effets de bords.
 */
abstract class  Game(){
  type G <: Game
  val user:String
  val password:String
  val users:List[String]
  val teaser:String
  val pace:Int
  val nb_turn:Int
  val nb_ant_per_player:Int
  val nb_player:Int
  val minimal_nb_players:Int
  val initial_energy:Int
  val initial_acid:Int
  val id:StringId
  val com_parse:Parse[G]

  
  /** Boucle principale qui doit être appelée pour jouer une partie
   *  Cette boucle adopte un style fonctionnelle pour éviter les effets de 
   *  bords. 
   */
  def main_loop():Unit={
 
       val queen = new Queen(com_parse)
       
       /** aux est la fonction récurssive qui fait le job
       *   Elle crée l'instance de wordMap et lance la routine (turn)
       *   de queen. 
       */
      def aux(loop:Int,parse:Parse[G],map:Option[WorldMap],obs:Option[Observations]):Unit={
         if(loop == 0)
           return
         val mapp = WorldMapHCreator.make(map,obs)
         val obss = queen.turn(mapp)
         aux(loop-1,com_parse,Some(mapp),Some(obss))
      }

      
      /**1 - rejoindre la partie*/
      try{
        com_parse.join
      }catch{
        case ex: AlredyJoined =>
            Thread.sleep(1000)
        case e:ServerErrorException => throw e
      } 
      
      /**2 - jouer */
      try{
        aux(nb_turn,com_parse,None,None)
      }catch{
        case ex: GameIsNotPlaying =>
            println("waiting for players")
            Thread.sleep(2000)
            main_loop()
        case e:ServerErrorException => throw e
      }
    }

  
   /** Permet de s'identifier avec les serveur
   *   si l'utilisateur n'est enregistré on l'enregistre et on ce connecte.
   */
  protected def forceAuth()={
      try{
        com_parse.auth(user, password)
      }catch{
        case e: UnknownUser =>{
            System.err.println(">>user doesn't exist. Program will request for register!")
            com_parse.register(user, password)
            com_parse.auth(user, password)
        }
        case e:ServerErrorException => throw e
      }
  }
}


/** Crée une partie */
class CreateGame( override val user:String,
                  override val password:String,
                  override val users:List[String],
                  override val teaser:String,
                  override val pace:Int,
                  override val nb_turn:Int,
                  override val nb_ant_per_player:Int,
                  override val nb_player:Int,
                  override val minimal_nb_players:Int,
                  override val initial_energy:Int,
                  override val initial_acid:Int)extends Game{
  
  /**Construction (initialisation de la partie)*/
  type G = CreateGame
  override val com_parse = new Parse[CreateGame](this)
  forceAuth()
  override val id = com_parse.newGame(  users,
                                         teaser,
                                         pace,
                                         nb_turn,
                                         nb_ant_per_player,
                                         nb_player,
                                         minimal_nb_players,
                                         initial_energy,
                                         initial_acid)
}


/** Rejoindre une partie */
class JoinGame(  override val user:String,
                 override val password:String,
                 override val users:List[String],
                 override val teaser:String,
                 override val pace:Int,
                 override val nb_turn:Int,
                 override val nb_ant_per_player:Int,
                 override val nb_player:Int,
                 override val minimal_nb_players:Int,
                 override val initial_energy:Int,
                 override val initial_acid:Int,
                 override val id:StringId)extends Game{
  
  /**Construction (initialisation de la partie)*/
  type G = JoinGame
  override val com_parse = new Parse[JoinGame](this)
  forceAuth()
}
