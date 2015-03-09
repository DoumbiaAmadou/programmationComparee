package game

import comm.Parse

abstract class  Game(){
  val user:String
  val password:String
  val users:String//List[String]//TODO remplacer String par User
  val teaser:String
  val pace:Int
  val nb_turn:Int
  val nb_ant_per_player:Int
  val nb_player:Int
  val minimal_nb_players:Int
  val intial_energy:Int
  val initial_acid:Int
  val id:Int//id du jeu
}

/*Game crée une partie*/
class CreateGame( override val user:String,
                  override val password:String,
                  override val users:String,
                  override val teaser:String,
                  override val pace:Int,
                  override val nb_turn:Int,
                  override val nb_ant_per_player:Int,
                  override val nb_player:Int,
                  override val minimal_nb_players:Int,
                  override val intial_energy:Int,
                  override val initial_acid:Int)extends Game{
  
  val parse = new Parse[CreateGame](this)
  //parse.verif_user()
  override val id = 999//TODO à remplacer par la vrai valeur
  def new_game()={
    
  }
  
}
