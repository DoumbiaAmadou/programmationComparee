import scalaj.http._
import spray.json._
import DefaultJsonProtocol._ 

class ConnectApi() {
  private val baseUrl = "http://yann.regis-gianas.org/antroid/"
  private val version = "0"

  //Création d'une requete basique
  def makeHttpRequest(requestName: String): HttpRequest = {
    val request: HttpRequest = Http(baseUrl + version + "/" + requestName)
    return request
  }
  // Envoie une requete avec la méthode POST
  def postRequest(requestName: String, params: Seq[(String, String)]): String = {
    val response: HttpResponse[String] = makeHttpRequest(requestName).postForm(params).asString
    return response.body
  }

  // Authentification d'un utilisateur
  def auth(login: String, password: String): String = {
    var params = Seq("login" -> login, "password" -> password)
    return postRequest("auth", params)
  }
	
	//Enregistrement d'un utilisateur
  def registerUser(login: String, password: String): String = {
    var params = Seq("login" -> login, "password" -> password)
    return postRequest("register", params)
  }

  //Creation d'un nouveau jeu
  def newGame(users: String, teaser: String, pace: String, nb_turn: Int, nb_ant_per_player: Int, nb_player: Int,
    minimal_nb_player: Int, initial_energy: Int, initial_acid: Int): String = {
    val response: HttpResponse[String] = makeHttpRequest("create?users="
      + users + "&teaser=" + teaser + "&pace=" + pace + "&nb_turn=" + nb_turn + "&nb_ant_per_player="
      +nb_ant_per_player + "&nb_player=" + nb_player + "&minimal_nb_player=" + minimal_nb_player 
      + "&initial_energy=" + initial_energy + "&initial_acid=" + initial_acid).asString
    return response.body
  }

  // Destruction d'une partie
  def destroyGame(idGame: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("destroy?id=" + idGame).asString
    return response.body
  }

  // Rejoindre une partie
  def joinAGame(idGame: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("join?id=" + idGame).asString
    return response.body
  }

  // status d'un game
  def status(idGame: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("status?id=" + idGame).asString
    return response.body
  }

  // Liste des jeux visibles
  def listGame(): String = {
    val response: HttpResponse[String] = makeHttpRequest("games").asString
    return response.body
  }
}
// Test connect
object test {
  def main(args: Array[String]) {
    val connection: ConnectApi = new ConnectApi()
    println("Hello, scala request!")
    // println(connection.registerUser("test1", "test1"))
    // println(connection.auth("test1", "test1"))
    // println(connection.destroyGame("17262510196880258110079859391070411975"))
    println(connection.newGame("all", "test", "10", 2, 3, 3, 2, 100, 100))
    // println(connection.listGame())
    // println(connection.joinAGame("4986356145915714886550832111036009180"))
    // println(connection.status("4986356145915714886550832111036009180"))
  }
}
