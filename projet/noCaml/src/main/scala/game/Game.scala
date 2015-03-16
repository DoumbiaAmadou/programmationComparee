package game

import aliases.Type._
import comm._
import comm.parse.Observations
import sys.process._
import world._
import ants.ia._

//TODO faire Une classe player des Threads pour les Games (peut être extends sur Game) ou peut utiliser les Actor pour synchroniser 2 joureurs

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
  
  /** permet de s'identifier avec les serveur
   *  si l'utilisateur n'est enregistré on l'enregistre et on ce connecte.
   */
  protected def force_auth()={
      try{
        com_parse.auth(user, password)
      }catch{
        case e: ServerErrorException =>{
          if(e.er.response.error_code == 598036704){//TODO faire un comme en c "#define"
            System.err.println(">>user doesn't exist. Program will request for register!")
            com_parse.register(user, password)
            com_parse.auth(user, password)
          }
          else{
            println(">>receive this error from server: "+e.er.response.error_msg)
          }
        }
      }
  }
  
  def main_loop()={
      /**rejoindre la partie*/
      try{
        com_parse.join
      }catch{
        case ex: ServerErrorException =>
          if (ex.er.response.error_code == 443034632){
            println("alredy in game")
          }else if(ex.er.response.error_code == 357629463){
            "sleep 1".!!//TODO à modifier
          }else{
            throw ex
          }
      } 
      
      val queen = new Queen(com_parse)
      
      def aux(loop:Int,parse:Parse[G],map:Option[WorldMap],obs:Option[Observations]):Unit={
         if(loop == 0)
           return
         val mapp = WorldMapHCreator.make(map,obs)
         println(mapp.ants)
         val obss = queen.turn(mapp)
         aux(loop-1,com_parse,Some(mapp),Some(obss))
      }
  
      aux(nb_turn,com_parse,None,None)
    }
    
    def test_loop(login:String,password:String,sleep:String)={//avant de pouvoir utiliser main loop
      sleep.!!//le sleep sert à tricher pour l'atomicité...
      com_parse.auth(login, password)
      com_parse.join
      val a = (0,Forward)::(1,Forward)::Nil
      def aux(loop:Int):Unit={
        if(loop == 0)
          return
        println("PLAYER "+login+"=====================\n\n")
        sleep.!!
        com_parse.auth(login, password)//attention pas atomic...
        com_parse.play(a)
        println("======================================\n")
        aux(loop-1)
      }
      aux(nb_turn)
    }
}

/**Game crée une partie*/
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
  
  /**Construction (initialisation de la partie)**************/
  type G = CreateGame
  override val com_parse = new Parse[CreateGame](this)
  force_auth()
  override val id = com_parse.new_game(  users,
                                         teaser,
                                         pace,
                                         nb_turn,
                                         nb_ant_per_player,
                                         nb_player,
                                         minimal_nb_players,
                                         initial_energy,
                                         initial_acid)
  /**********************************************************/

}

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
  
  /**Construction (initialisation de la partie)**************/
  type G = JoinGame
  override val com_parse = new Parse[JoinGame](this)
  force_auth()
  /**********************************************************/
  /*
  override def main_loop()={
    try{
      super.main_loop()
    }catch{
      case ex: ServerErrorException =>
        if (ex.er.response.error_code == 443034632){//already in game
          main_loop()
        }
    }      
  }
  */
}


/*
object CreateGame2players__{//XXX version sans les threads
  
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 100
    val nb_ants=25
    val nb_players = 2
    val minimal_players = 2
    val g1 = new CreateGame("koko","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100)//XXX ici il faut changer le nom car j'ai le cookie de l'utilisateur koko
    
    val p1 = g1.com_parse
    println("STATUS 1")
    p1.status
    
    
    p1.join
    println("STATUS 2 (player 1 join)")
    val id = g1.id
    
    val g2 = new JoinGame("ikki","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100,id)
    val p2 = g2.com_parse
    p2.join
    
    
    println("STATUS 3 (player 2 join)")
    p1.status
    
    //p1.destroy
    //println("STATUS 4 destroy")
    //p1.status

    val actions = (0, Forward)::(1, Left)::(2, Forward)::(3, Forward)::(4, Forward)::(5, Forward)::(6, Forward)::(7, Forward)::(8, Forward)::(9, Forward)::(10, Forward)::(11, Forward)::(12, Forward)::(13, Forward)::(14, Forward)::(15, Forward)::(16, Forward)::(17, Forward)::(18, Forward)::(19, Forward)::(20, Forward)::(21, Forward)::(22, Forward)::(23, Forward)::(24, Forward)::Nil
    val sleep = "sleep 1"

    println("\n\n\n\n\nMAIN LOOP\n\n\n")
    
    for(x <- 1 to nb_turn){
      
      
      p1.auth("koko", "atat")
      //p1.whoami
      p1.play(actions)
      //p1.status
      /*
      p2.auth("ikki", "atat")
      p2.whoami
      p2.play(actions)
      */
      //sleep.!!//XXX ceci est un sleep à l'arache à ne pas utiliser
      
      println("\n\n\nLOOP\n\n\n")
    }
    
  }

}
*/

/*
object CreateGame1player{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 25
    val nb_ants=25
    val nb_players = 1
    val minimal_players = 1
    val g1 = new CreateGame("koko","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100)//XXX ici il faut changer le nom car j'ai le cookie de l'utilisateur koko
    g1.main_loop()
    
  }
}
*/


object CreateGame2players{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 25
    val nb_ants=25
    val nb_players = 2
    val minimal_players = 2
    val g1 = new CreateGame("koko","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100)//XXX ici il faut changer le nom car j'ai le cookie de l'utilisateur koko
    g1.main_loop()
    
  }
}


object JoinGame1player{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 25
    val nb_ants=25
    val nb_players = 2
    val minimal_players = 2
    val id ="136252565941941454183945855112273920"
    val g2 = new JoinGame("ikki","atat",List("all"),"test_create_Game!",10,nb_turn,nb_ants,nb_players,minimal_players,1,100,id)    
    g2.main_loop()
    
  }
}