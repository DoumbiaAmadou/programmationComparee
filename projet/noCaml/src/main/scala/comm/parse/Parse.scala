package comm

import scala.collection.immutable._
import aliases.Type._
import game._
import comm.parse._

/**Exception pour les messages d'erreur du serveur
 */
sealed case class ServerErrorException(error_code:Int,error_msg:String) extends Exception(error_msg)

class Parse[T <: Game](val game:T){
  private var actions:SortedMap[AntNum,Command] = new TreeMap
  
  private def error_msg(json_string:String)={
    println("message from server:"+json_string)//afficher les infos du serveur
    val err = JSONParse.msg_serveur(json_string)
    if(err.error_code != 0)
      throw ServerErrorException(err.error_code,err.error_msg)
  }
  
  def register(login:String,password:String)={
    val json = Network.register(login,password)
    error_msg(json)
  }
  
  def auth(login:String,password:String)={
    val json = Network.auth(login,password)
    error_msg(json)
  }
  
  /**new_game : créer une nouvelle partie
   * retourne l'id de la partie
   * */
  def new_game(  users:List[String],
                 teaser:String, 
                 pace:Int, nb_turn:Int,
                 nb_ant_per_player:Int,
                 nb_player:Int, 
                 minimal_nb_players:Int,
                 intial_energy:Int,
                 initial_acid:Int):String={
    val json = Network.new_game(  users,
                                  teaser,
                                  pace,
                                  nb_turn,
                                  nb_ant_per_player,
                                  nb_player,
                                  minimal_nb_players,
                                  intial_energy,
                                  initial_acid)
    error_msg(json)
    return JSONParse.msg_create(json).response.identifier
  }
  
  def status={
     val json = Network.status(game.id)
     error_msg(json)
  }
  
  def join={
     val json = Network.join(game.id)
     error_msg(json)
  }
  
  def destroy={
     val json = Network.destroy(game.id)
     error_msg(json)
  }
  
  def observe(ant_id:Int)={
     val json = Network.observe(game.id, ant_id)
     error_msg(json)
  }
  
  def log={
     val json = Network.log(game.id)
     error_msg(json)
  }
  
  def api = atomic("api")
  
  def whoami = atomic("whoami")
  
  def logout = atomic("logout")
  
  def games = atomic("games")
  
  def atomic(atom:String)={
     val json = Network.atomic(atom)
     error_msg(json)
  }
  
  /**envoyer toutes les actions*/
  def play():Observations={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parse_action(kv._1,kv._2)+",")
    println("CHAINE : "+cmds)//TODO info debug à retirer
    val json = Network.play(game.id, cmds)
    error_msg(json)
    return JSONParse.observation(json)
    //TODO une fois play lancé il faut soit mettre à la poubelle les actions soit faire en sorte que l'on en ait de nouvelles
  }
  
  /**parse l'action sur la forme "<chiffre>:<command>"*/
  private def parse_action(ant_num:AntNum, command:Command):String=
    ant_num.toString+":" + command.parsed

  /**Ajoute une ou plusieurs actions pour une fourmie*/
  def add_action(ant_num:Int,command:Command)=
    actions = actions + ((ant_num,command))

}

/*test*/
object ParseTest{
  def main(args: Array[String]):Unit={
    val game = new CreateGame("user","pass",List(""),"",0,0,0,0,0,0,0)
    println("game id ="+game.id)
    val parse = new Parse(game)
    parse.add_action(0, Atk(20))
    parse.add_action(2, Left)
    parse.add_action(1, Right)
    //parse.add_action(7, Hack(Rest))
    //parse.add_action(5, Hack(Store("x",N(12))))
    val l = List(  (None,Rest),
                   (Some("BB"),Store("x",N(44))),
                   (Some("AA"),Left),
                   (None,Right),
                   (None,Store("y",Add(1,2)))
                )
    parse.add_action(10, Hack(l))
    parse.play()
  }
}

