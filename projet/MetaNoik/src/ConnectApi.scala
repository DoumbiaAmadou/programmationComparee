import scalaj.http._

class ConnectApi {

  private val baseUrl = "http://yann.regis-gianas.org/antroid/"
  private val version = "0"
  //Création d'une requete basique
  def makeHttpRequest(requestName: String): HttpRequest = {
    val request: HttpRequest = Http(baseUrl + version + "/" + requestName + "?")
    return request
  }
  // Envoie une requete avec la méthode POST
  def postRequest(requestName: String, params: Seq[(String, String)]): String = {
    val response: HttpResponse[String] = makeHttpRequest(requestName).postForm(params).asString
    return response.body
  }

  //Envoie une requete avec la methode GET (sans parametres)
  def getRequest(requestName: String): String = {
    val response: HttpResponse[String] = makeHttpRequest(requestName).asString
    return response.body
  }

  //Enregistrement d'un utilisateur
  def registerUser(login: String, password: String): String = {
    var params = Seq("login" -> login, "password" -> password)
    return postRequest("register", params)
  }

  // Authentification d'un utilisateur
  def auth(login: String, password: String): String = {
    var params = Seq("login" -> login, "password" -> password)
    return postRequest("auth", params)
  }

  //Deconnexion
  def logout(): String = {
    val response: HttpResponse[String] = makeHttpRequest("logout").asString
    return response.body
  }

  //Creation d'un nouveau jeu
  def newGame(users: String, teaser: String, pace: String, nb_turn: Int, nb_ant_per_player: Int, nb_player: Int,
    minimal_nb_player: Int, initial_energy: Int, initial_acid: Int): String = {
    val response: HttpResponse[String] = makeHttpRequest("create?users="
      + users + "&teaser=" + teaser + "&pace=" + pace + "&nb_turn=" + nb_turn + "&nb_ant_per_player="
      + nb_ant_per_player + "&nb_player=" + nb_player + "&minimal_nb_player=" + minimal_nb_player
      + "&initial_energy=" + initial_energy + "&initial_acid=" + initial_acid).asString
    return response.body
  }

  // Rejoindre une partie
  def joinAGame(idGame: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("join?id=" + idGame).asString
    return response.body
  }

  // Liste des jeux visibles
  def listGame(): String = {
    return getRequest("games")
  }

  //Statut du jeu
  def statusGame(idGame: String): String = {
    return getRequest("status?id=" + idGame)
  }

  // Destruction d'une partie
  def destroyGame(idGame: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("destroy?id=" + idGame).asString
    return response.body
  }

  //Statut de l'utilisateur dans le serveur
  def whoami(): String = {
    return getRequest("whoami")
  }

  /*Commandes pour les fourmis*/

  def antCmd(idGame: String, action: String): String = {
    val response: HttpResponse[String] = makeHttpRequest("play?id=" + idGame + "&cmds=left").asString
    return response.body
  }

  def left(idGame: String): String = {
    return antCmd(idGame, "left")
  }

  def right(idGame: String): String = {
    return antCmd(idGame, "right")
  }

  def forward(idGame: String): String = {
    return antCmd(idGame, "forward")
  }

  def rest(idGame: String): String = {
    return antCmd(idGame, "rest")
  }

  def attack(idGame: String, level: Int): String = {
    return antCmd(idGame, "attack@" + level)
  }

}
// Test connexion
object test {
  def main(args: Array[String]) {
    val connection: ConnectApi = new ConnectApi()
    println("Hello, scala request!")
    //println(connection.registerUser("test1", "test1"))
    println(connection.auth("test1", "test1"))
    println(connection.newGame("all", "test", "10", 2, 3, 3, 2, 100, 1))
    //println(connection.listGame())
    println(connection.joinAGame("4986356145915714886550832111036009180"))
    println(connection.left("4986356145915714886550832111036009180"))
  }
}
