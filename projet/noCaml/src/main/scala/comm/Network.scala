package comm

import sys.process._

/*toute ressemblance avec un code existant ne saurait être que fortuite*/
object Network {
  
  private val version_num = 0
  private val antroide_url="https://yann.regis-gianas.org/antroid/"+version_num
  private val cookies = ".cookies"
  private val curl ="curl -s -k --cookie "+cookies+" --cookie-jar "+cookies
  
  private def error(msg:String)=
    System.err.println(msg)
  
  def register(login:String,password:String):String={
    val cmd = curl+" "+antroide_url+"/register -d login="+login+" -d password="+password
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def auth(login:String,password:String):String={
    val cmd = curl+" "+antroide_url+"/auth -X POST -d login="+login+" -d password="+password
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!! 
  }
  
  def new_game(  users:List[String],
                 teaser:String, 
                 pace:Int, nb_turn:Int,
                 nb_ant_per_player:Int,
                 nb_player:Int, 
                 minimal_nb_players:Int,
                 intial_energy:Int,
                 initial_acid:Int):String={
    var cmd = curl+" -G "+antroide_url+"/create"
        cmd = cmd+" -d users="+users.foldLeft("")((acc,s)=> acc+s+" ")//.foreach { x => x+" " }//TODO à vérifier
        cmd = cmd+" --data-urlencode teaser="+teaser
        cmd = cmd+" -d pace="+pace
        cmd = cmd+" -d nb_turn="+nb_turn
        cmd = cmd+" -d nb_ant_per_player="+nb_ant_per_player
        cmd = cmd+" -d nb_player="+nb_player
        cmd = cmd+" -d minimal_nb_player="+minimal_nb_players
        cmd = cmd+" -d initial_energy="+initial_acid
        cmd = cmd+" -d initial_acid="+initial_acid
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def status(game_id:String):String={
    val cmd = curl+" -G "+antroide_url+"/status -d id="+game_id
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def join(game_id:String):String={
    val cmd = curl+" -G "+antroide_url+"/join -d id="+game_id
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def destroy(game_id:String):String={
    val cmd = curl+" -G "+antroide_url+"/destroy -d id="+game_id
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def play(game_id:String,cmds:String):String={
    val cmd = curl+" -G "+antroide_url+"/play -d id="+game_id+" -d cmds="+cmds
    println("PLAY cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
  
  def observe(game_id:String,ant_id:Int):String={
    val cmd = curl+" -G "+antroide_url+"/observe id="+game_id+" -d antid="+ant_id
    return cmd.!!
  }
  
  def log(game_id:String):String={
    val cmd = curl+" -G "+antroide_url+"/log -d id="+game_id
    return cmd.!!
  }
  
  def atomic(atom:String):String={
    val cmd = curl+" "+antroide_url+"/"+atom
    println("cmd=[ "+cmd+" ]\n\n")//debug
    return cmd.!!
  }
}

/*tests*/
object NetworkTest{
   def main(args: Array[String])={
     println(Network.atomic("whoami"))
   }
}