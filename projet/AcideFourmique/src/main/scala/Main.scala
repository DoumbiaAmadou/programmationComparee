import server._
import antCommand._

object Main {

  def main(args: Array[String]): Unit = {
    val login = "tset"
    val passwd = "tset"
    val pace = 30
    val nb_turn = 60
    val nb_ant = 2
    val nb_player = 1
    val minimal_nb_player = 1

    Antroid.authenticate(login, passwd)
    val game_id = Antroid.create(login, "test",
      pace, nb_turn, nb_ant, nb_player, minimal_nb_player)

    Antroid.join(game_id)

    /* Code a introduire dans une fourmi zombie.
     * Le zombie verifie juste s'il y a un rocher en face de lui:
     * si c'est le cas il tourne a gauche et recommence la verification,
     * sinon il avance.
     * La fourmi zombie recommence l'operation jusqu'a sa mort.
     * 
     * L'operateur := construit l'instruction Store.
     * Cet operateur est fourni par la classe Var. */
    val code = new Code(List(
      // primitive see_ant et entiers non reconnu par le serveur
      (Label("LOOP"),  Left()),
      (None,           Var("x") := Apply(See())),
      (None,           Jumpifz(Var("x"), new Label("LOOP"))),
      (None,           Forward()),
      (None,           Jump(new Label("LOOP")))
    ))

    // Un tour de boucle correspond a trois tours de jeu 
    for (round <- 1 to nb_turn by 3) {
      println("--------------- ROUND " + round + " ----------------")
      Antroid.play(game_id, List(Forward() << 0, Forward() << 1))
      Antroid.play(game_id, List(Attack(10) << 0, Hack(code) << 1))
      Antroid.play(game_id, List(Hack(code) << 0, Attack(10) << 1))
      for (status <- Antroid.status(game_id))
        println("score: " + status.score)
    }

    for (status <- Antroid.status(game_id)) {
      println("=========================================")
      println("game status: " + status.status("status"))
      println("score: " + status.score)
    }

    Antroid.destroy(game_id)
    Antroid.logout()
  }
}
