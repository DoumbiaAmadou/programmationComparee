package game

import comm.Parse

/*Game contient tout les paramètres de la partie*/
class Game( val users:String,
            val teaser:String,
            val pace:Int,
            val nb_turn:Int,
            val nb_ant_per_player:Int,
            val nb_player:Int,
            val minimal_nb_players:Int,
            val intial_energy:Int,
            val initial_acid:Int){
  
  private val parse = new Parse(this)
  private var idx = 0
  
  def id = idx
  def new_game()={}
  
}