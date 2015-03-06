package comm

import game.Game
import scala.collection.immutable._
import aliases.Type._;

class Parse(val game:Game){
  private var actions:SortedMap[AntNum,Command] = new TreeMap
  
  def register(login:String,password:String)={
    val json = Network.register(login,password)
  }
  
  def auth(login:String,password:String)={
    val json = Network.auth(login,password)
  }
  
  def new_game(  users:String,
                 teaser:String, 
                 pace:Int, nb_turn:Int,
                 nb_ant_per_player:Int,
                 nb_player:Int, 
                 minimal_nb_players:Int,
                 intial_energy:Int,
                 initial_acid:Int)={
    val json = Network.new_game(  users,teaser,pace,nb_turn,
                                  nb_ant_per_player,nb_player,
                                  minimal_nb_players,intial_energy,
                                  initial_acid
                               )
  }
  
  def status()={
     val json = Network.status(game.id)
  }
  
  def join()={
     val json = Network.join(game.id)
  }
  
  def destroy()={
     val json = Network.destroy(game.id)
  }
  
  def observe(ant_id:Int)={
     val json = Network.observe(game.id, ant_id)
  }
  
  def log()={
     val json = Network.log(game.id)
  }
  
  def atomic(atom:String)={
     val json = Network.atomic(atom)
  }
  
  /**envoyer toutes les actions*/
  def play()={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parse_action(kv._1,kv._2)+"; ")
    println("CHAINE : "+cmds)//TODO test à retirer
    //network.play(game.id, cmds)//TODO à remettre
  }
  
  /**parse l'action sur la forme "<chiffre>:<command>"*/
  private def parse_action(ant_num:AntNum, command:Command):String=
    ant_num.toString+":" + command.parsed

  /**Ajoute une action pour une fourmie*/
  def add_action(ant_num:Int,command:Command)=
    actions = actions + ((ant_num,command))

  



}





object Parse{
  def main(args: Array[String])={
    val game = new Game("","",0,0,0,0,0,0,0)
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