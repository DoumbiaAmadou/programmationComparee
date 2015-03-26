import server._
import antCommand._
import logGame._

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
     * L'operateur ':=' construit l'instruction Store.
     * Cet operateur est fourni par la classe Var. */
    val code = new Code(List(
      // primitive see_ant et entiers non reconnu par le serveur
      (Label("LOOP"),  Left()),
      (None,           Var("x") := Apply(SeeAnt())),
      (None,           Jumpifz(Var("x"), new Label("LOOP"))),
      (None,           Forward()),
      (None,           Jump(new Label("LOOP")))
    ))

    /* Un tour de boucle correspond a trois tours de jeu.
     * Joue une commande autant de fois qu'il y a de tour */
    for (round <- 1 to nb_turn by 3) {
      println("--------------- ROUND " + round + " ----------------")
      Antroid.play(game_id, List(Forward() << 0, Forward() << 1))
      Antroid.play(game_id, List(Attack(10) << 0, Hack(code) << 1))
      Antroid.play(game_id, List(Hack(code) << 0, Attack(10) << 1))
      /* la construction avec for permet d'effectuer le corps de la boucle
       * seulement si gamestat n'est pas None */
      for (gamestat <- Antroid.status(game_id))
        println("score: " + gamestat.score)
    }

    `wait until game is over`(game_id)

    // Envoi le log de la partie au serveur de log (programme ecrit en C)
    GameLog.sendLog(Antroid.log(game_id))

    for (gamestat <- Antroid.status(game_id)) {
      println("=========================================")
      println("game status: " + gamestat.status("status"))
      println("score: " + gamestat.score)
    }

    Antroid.destroy(game_id)
    Antroid.logout()
  }

  //attente de la fin d'une partie (attente passive)
  private def `wait until game is over`(game_id: String) = {
    def `is the game over?`(status: String): Unit = status match {
      case "over" => ()
      case _ => {
        Thread.sleep(1000)
        for (gamestat <- Antroid.status(game_id))
          `is the game over?`(gamestat.status("status"))
      }
    }
    print("Waiting for the end of the game... ")
    for (gamestat <- Antroid.status(game_id))
      `is the game over?`(gamestat.status("status"))
    println("OK")
  }
}
