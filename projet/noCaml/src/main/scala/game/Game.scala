package game

import comm._
import sys.process._

abstract class  Game(){
  val user:String
  val password:String
  val users:List[String]
  val teaser:String
  val pace:Int
  val nb_turn:Int
  val nb_ant_per_player:Int
  val nb_player:Int
  val minimal_nb_players:Int
  val intial_energy:Int
  val initial_acid:Int
  val id:String//id du jeu
}

/*Game crée une partie*/
class CreateGame( override val user:String,
                  override val password:String,
                  override val users:List[String],
                  override val teaser:String,
                  override val pace:Int,
                  override val nb_turn:Int,
                  override val nb_ant_per_player:Int,
                  override val nb_player:Int,
                  override val minimal_nb_players:Int,
                  override val intial_energy:Int,
                  override val initial_acid:Int)extends Game{
  val com_parse = new Parse[CreateGame](this)
  try{
    com_parse.auth(user, password)
  }catch{
    case e: ServerErrorException =>{
      if(e.error_code == 202165063){
        System.err.println(">>user doesn't exist. Program will request for register!")
        com_parse.register(user, password)
        com_parse.auth(user, password)
      }
      else{
        println(">>>error")
      }
    }
  }
  override val id = com_parse.new_game(users, teaser, pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_players, intial_energy, initial_acid)  
}

object CreateGame{
  def main(args: Array[String])={
    println("Creation de la partie")
    val t = new CreateGame("koko","atat",List("koko"),"test_create_Game!",10,100,5,1,1,1,100)//XXX ici il faut changer le nom car j'ai le cookie de l'utilisateur koko
    val com_parse = t.com_parse
    
    println("game id ="+t.id)
    
    com_parse.add_action(0, Forward)
    com_parse.add_action(1, Left)
    com_parse.join
    
    
    val sleep = "sleep 1"
    for(x <- 1 to 3){
      val turn = com_parse.play.turn
      println("turn="+turn)
      sleep.!!//XXX ceci est un sleep à l'arache à ne pas utiliser
    }
    com_parse.destroy
    com_parse.logout
  }
}