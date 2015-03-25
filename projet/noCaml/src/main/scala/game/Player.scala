package game

import aliases.Type._
import sys.process._

/**
 * Cette classe à pour but simuler une partie avec de vrai joueurs localement.
 * Ceci afin de pouvoir faire des tests.
 * 
 * Note : Il y a des problème de synchronisation des threads.
 */
class PlayerCreate( val user:String,
                    val password:String,
                    val users:List[String],
                    val teaser:String,
                    val pace:Int,
                    val nb_turn:Int,
                    val nb_ant_per_player:Int,
                    val nb_player:Int,
                    val minimal_nb_players:Int,
                    val initial_energy:Int,
                    val initial_acid:Int) extends Runnable{
  
  val g = new CreateGame(user,password,users,
                         teaser,pace,nb_turn,
                         nb_ant_per_player,nb_player
                         ,minimal_nb_players,
                         initial_energy,initial_acid)
  
  override def run()={
    println("create run")
    g.main_loop()
    //g.test_loop(user,password, "echo ''")
  }
}

class PlayerJoin( val user:String,
                  val password:String,
                  val users:List[String],
                  val teaser:String,
                  val pace:Int,
                  val nb_turn:Int,
                  val nb_ant_per_player:Int,
                  val nb_player:Int,
                  val minimal_nb_players:Int,
                  val initial_energy:Int,
                  val initial_acid:Int,
                  val id:StringId) extends Runnable{
  
  val g = new JoinGame(user,password,users,teaser,pace,nb_turn,nb_ant_per_player,nb_player,minimal_nb_players,initial_energy,initial_acid,id)
  
  override def run()={
    println("join run")
    g.main_loop()
    //g.test_loop(user,password,"sleep 1")
  }
}


object Test2PlayersOneCumputer{
  def main(args: Array[String]):Unit={
    val nb_turn = 100
    val nb_ants=25
    val nb_players = 2
    val minimal_players = 2
    val pc = new PlayerCreate("koko","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100)
    val pj = new PlayerJoin("ikki","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100,pc.g.id)
    pc.run()
    pj.run()
  }
}