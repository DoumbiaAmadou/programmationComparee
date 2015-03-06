package game

import comm.Parse

/*Game contient tout les paramètres de la partie*/
class Game( 
        val users:String,
        val teaser:String,
        val pace:Int,
        val nb_turn:Int,
        val nb_ant_per_player:Int,
        val nb_player:Int,
        val minimal_nb_players:Int,
        val intial_energy:Int,
        val initial_acid:Int){
  
  val parse = new Parse(this)
  //val party = parse.create_game
  private var idx = 0//replacer 0 par party.id //correspond à l'id de la partie (il faut l'initialiser
  
  def id = idx
  def new_game()={
    
  }
  
}