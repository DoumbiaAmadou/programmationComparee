import server._
import antCommand._

object Main {

  def main(args: Array[String]): Unit = {
    val login = "tset"
    val passwd = "tset"
    val pace = 30
    val nb_turn = 60
    val nb_ant = 2

    Antroid.register("sett", "sett")
    Antroid.authenticate(login, passwd)
    val game_id =
      Antroid.create(login + ",sett", "test", pace, nb_turn, nb_ant, 1,1)

    Antroid.join(game_id)

    //code a introduire dans une fourmi zombie
    val code = new Code(List(
      (Label("START"), Fork()),
      (None,           Fork()),
      (Label("END"),   Fork())
    ))

    for (round <- 1 to nb_turn by 3) {
      println("--------------- ROUND " + round + " ----------------")
      Antroid.play(game_id, List(Forward() << 0, Forward() << 1))
      Antroid.play(game_id, List(Attack(10) << 0, Attack(10) << 1))
      Antroid.play(game_id, List(Hack(code) << 0, Right() << 1))
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
