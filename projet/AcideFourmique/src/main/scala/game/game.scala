package game 

case class Game(game_description: GameDescription)

case class GameDescription(
  identifier: String,
  creation_date: String,
  creator: String,
  teaser: String)

case class GameStatus(
  creator: String,
  creation_date: String,
  teaser: String,
  visibility: String,
  nb_ant_per_player: Int,
  pace: Int,
  initial_energy: Int,
  initial_acid: Int,
  players: List[String],
  score: Map[String,String],
  status: Map[String,String],
  turn: Int)
