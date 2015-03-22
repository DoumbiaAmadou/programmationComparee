package comm

import scala.collection.immutable._
import aliases.Type._
import game._
import comm.parse._
import ErrorCode._


/** Parse permet est l'interface de communication avec le serveur.
 *  
 *  Note 1: Une instance de Parse doit être créer avec une instance de Game.
 *  
 *  Note 2: Le code actuelle affiche tout les messages du serveur. Le but
 *          étant de les faire disparaitres une fois que tout aura été testé.
 *          
 *  
 *  @param game est un sous type de Game (soit CreateGame soit JoinGame)
 */
class Parse[T <: Game](val game:T){
  
  /** créer une nouvelle partie
   *  @return  retourne l'id de la partie
   */
  def newGame(  users:List[String],
                 teaser:String, 
                 pace:Int,
                 nb_turn:Int,
                 nb_ant_per_player:Int,
                 nb_player:Int, 
                 minimal_nb_players:Int,
                 intial_energy:Int,
                 initial_acid:Int):StringId={
    val json = Network.new_game(  users, teaser, pace,
                                  nb_turn, nb_ant_per_player,
                                  nb_player, minimal_nb_players,
                                  intial_energy, initial_acid)
    serverError("new_game",json)
    return JSONParse.msgCreate(json).response.identifier
  }
  
  /** envoyer(jouer) toutes les actions passées en paramètre au serveur
   *  @param actions  liste d'actions
   */
  def play(actions:List[(AntNum,Command)]):Observations={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parseAction(kv._1,kv._2)+",")
    val json = Network.play(game.id, cmds)
    serverError("play",json)
    return JSONParse.observation(json)
  }
  
  def register(login:String,password:String)={
    val json = Network.register(login,password)
    serverError("register",json)
  }
  
  def auth(login:String,password:String)={
    val json = Network.auth(login,password)
    serverError("auth",json)
  }
  
  def status={
     val json = Network.status(game.id)
     serverError("status",json)
  }
  
  def join={
     val json = Network.join(game.id)
     serverError("join",json)
  }
  
  def destroy={
     val json = Network.destroy(game.id)
     serverError("destroy",json)
  }
  
  def observe(ant_id:Int)={
     val json = Network.observe(game.id, ant_id)
     serverError("observe",json)
  }
  
  def log={
     val json = Network.log(game.id)
     serverError("log",json)
  }
  
  def atomic(atom:String)={
     val json = Network.atomic(atom)
     serverError("atom",json)
  }
  
  def api = atomic("api")
  
  def whoami = atomic("whoami")
  
  def logout = atomic("logout")
  
  def games = atomic("games")
  
  /** Affiche les réponses du serveur (utile pour les développeurs) 
   *   Lance une exception en cas de message d'erreur   
   */
  private def serverError(rep_from:String, json_string:String)={
    println("Function<"+rep_from+"> message from server:"+json_string)
    val err = JSONParse.msgServer(json_string)
    err match{
      case err:ErrorResponse => 
        err.response.error_code match{
          case ALREADY_JOINED      => throw new AlredyJoined(err)
          case GAME_IS_NOT_PLAYING => throw new GameIsNotPlaying(err)
          case UNKNOWN_USER        => throw new UnknownUser(err)
          case _                   => throw new ServerErrorException(err)
        }
        
      case _=>/*pas d'erreur*/
    }
  }
  
  /** Note : code utilisée par la méthode play
   *  @return retourne l'action sous la forme "<num fourmi>:<command>"
   */
  private def parseAction(ant_num:AntNum, command:Command):String=
    ant_num.toString+":" + command.parsed
    
}



/** ErrorCode contient les correspondances aux différents codes d'erreur du
 *  serveur. Contient aussi les exceptions correspondantes.
 *  
 *  Remarque on pourrait ce baser aussi sur le message d'erreur
 *  (qui ne change pas au code)
 */
object ErrorCode{
  val ALREADY_JOINED = 443034632
  val MUST_BE_LOGGED = 32403037
  val INVALID_GAME_IDENTIFIER = 513589683
  val GAME_IS_NOT_PLAYING = 357629463
  val UNKNOWN_USER = 502441794
  //...
  
  /**Exception pour les messages d'erreur du serveur */
  sealed case class ServerErrorException(er:ErrorResponse) 
                    extends Exception(er.response.error_msg)
  
  sealed class AlredyJoined(er:ErrorResponse)
                    extends ServerErrorException(er)
  
  sealed class GameIsNotPlaying(er:ErrorResponse)
                    extends ServerErrorException(er)
  
  sealed class UnknownUser(er:ErrorResponse)
                    extends ServerErrorException(er)
}

