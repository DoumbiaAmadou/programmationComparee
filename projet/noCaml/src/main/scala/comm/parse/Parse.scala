package comm

import scala.collection.immutable._
import aliases.Type._
import game._
import comm.parse._

/**Exception pour les messages d'erreur du serveur */
sealed case class ServerErrorException(er:ErrorResponse) extends Exception(er.response.error_msg)

class Parse[T <: Game](val game:T){
  
  /**new_game
   * 
   * créer une nouvelle partie
   * @return  retourne l'id de la partie
   * */
  def new_game(  users:List[String],
                 teaser:String, 
                 pace:Int, nb_turn:Int,
                 nb_ant_per_player:Int,
                 nb_player:Int, 
                 minimal_nb_players:Int,
                 intial_energy:Int,
                 initial_acid:Int):StringId={
    val json = Network.new_game(  users,
                                  teaser,
                                  pace,
                                  nb_turn,
                                  nb_ant_per_player,
                                  nb_player,
                                  minimal_nb_players,
                                  intial_energy,
                                  initial_acid)
    server_error("new_game",json)
    println("json = "+json)
    return JSONParse.msg_create(json).response.identifier
  }
  
  /**play
   * 
   * envoyer(jouer) toutes les actions passées en paramètre
   * */
  def play(actions:List[(AntNum,Command)]):Observations={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parse_action(kv._1,kv._2)+",")
    val json = Network.play(game.id, cmds)
    server_error("play",json)
    return JSONParse.observation(json)
  }
  
  def register(login:String,password:String)={
    val json = Network.register(login,password)
    server_error("register",json)
  }
  
  def auth(login:String,password:String)={
    val json = Network.auth(login,password)
    server_error("auth",json)
  }
  
  def status={
     val json = Network.status(game.id)
     server_error("status",json)
  }
  
  def join={
     val json = Network.join(game.id)
     server_error("join",json)
  }
  
  def destroy={
     val json = Network.destroy(game.id)
     server_error("destroy",json)
  }
  
  def observe(ant_id:Int)={
     val json = Network.observe(game.id, ant_id)
     server_error("observe",json)
  }
  
  def log={
     val json = Network.log(game.id)
     server_error("log",json)
  }
  
  def atomic(atom:String)={
     val json = Network.atomic(atom)
     server_error("atom",json)
  }
  
  def api = atomic("api")
  
  def whoami = atomic("whoami")
  
  def logout = atomic("logout")
  
  def games = atomic("games")
  
   /**server_error
   * 
   * affiche les réponses du serveur (utile pour les développeurs) 
   * Lance une exception en cas de message d'erreur
   */
  private def server_error(rep_from:String, json_string:String)={
    //println("Function<"+rep_from+"> message from server:"+json_string)//afficher les infos en provenance du serveur
    val err = JSONParse.msg_server(json_string)
    err match{
      case err:ErrorResponse => throw ServerErrorException(err)
      case _=>/*ne rien faire*/
    }
  }
  
  /**parse_action
   * 
   * utilisée par "play"
   * @return : retourne l'action sous la forme "<num fourmi>:<command>"
   */
  private def parse_action(ant_num:AntNum, command:Command):String=
    ant_num.toString+":" + command.parsed
    
}