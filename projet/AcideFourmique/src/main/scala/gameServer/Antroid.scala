package gameServer

import CurlService._
import antCommand._
import game._

import net.liftweb.json._


object Antroid {

  private implicit val formats = DefaultFormats

  private val api_version = 0
  private val server =
    initServer("https://yann.regis-gianas.org/antroid/" + api_version)

  
  /** Demande au serveur l'inscription de l'utilisateur
    * associe a 'login' et 'passwd' */
  def register(login: String, passwd: String): Unit = {
    val request = createPostRequest(server + "/register")
    request.addParameters( Map(
      "login"    -> login,
      "password" -> passwd
    ))

    val result = request.send()
    interpret(result)
  }

  /** Permet la connexion d'un utilisateur */
  def authenticate(login: String, passwd: String): Unit = {
    val request = createPostRequest(server + "/auth")
    request.addParameters( Map(
      "login"    -> login,
      "password" -> passwd
    ))

    val result = request.send()
    interpret(result)
  }

  /** Deconnexion de l'utilisateur en cours */
  def logout(): Unit = {
    val request = createGetRequest(server + "/logout")
    val result = request.send()
    interpret(result)
  }

  /** Retourne la liste des parties disponibles */
  def games(): List[GameDescription] = {
    val request = createGetRequest(server + "/games")
    val result = request.send()
    val game_list = interpret(result) match {
      case Some(r) => (r \ "games").extract[List[Game]]
      case None => List()
    }
    game_list map { case Game(desc) => desc }
  }

  /** Permet la creation d'une partie. Certains parametres sont optionnels.
    * Retourne l'identifiant associe a cette partie */
  def create(
    users: String,
    teaser: String,
    pace: Int = 10,
    nb_turn: Int = 10,
    nb_ant_per_player: Int = 3,
    nb_player: Int = 2,
    minimal_nb_player: Int = 2,
    initial_energy: Int = 100,
    initial_acid: Int = 100
  ): String = {
    
    val request = createGetRequest(server + "/create")
    request.addParameters( Map(
      "users"		  -> users,
      "teaser"		  -> teaser,
      "pace"		  -> pace.toString,
      "nb_turn"		  -> nb_turn.toString,
      "nb_ant_per_player" -> nb_ant_per_player.toString,
      "nb_player"	  -> nb_player.toString,
      "minimal_nb_player" -> minimal_nb_player.toString,
      "initial_energy"	  -> initial_energy.toString,
      "initial_acid"	  -> initial_acid.toString
    ))

    val result = request.send()
    interpret(result) match {
      case Some(r) => (r \ "identifier").extract[String]
      case None => ""
    }
  }
  
  /** Permet de rejoindre une partie */
  def join(game_id: String): Unit = {
    val request = createGetRequest(server + "/join")
    request.addParameters( Map("id" -> game_id) )
    val result = request.send()
    interpret(result)
  }
  
  /** Renvoie le status d'une partie. Retourne un type Option.
    * 
    * @TODO: Fonctionne soit pour les parties publics,
    * soit pour les parties privees, mais pas les deux
    * (selon le type de l'attribut visibility de la classe GameStatus) */
  def status(game_id: String): Option[GameStatus] = {
    val request = createGetRequest(server + "/status")
    request.addParameters( Map("id" -> game_id) )
    val result = request.send()
    interpret(result) match {
      case Some(r) => Some((r \ "status").extract[GameStatus])
      case None => None
    }
  }

  /** Detruit une partie */
  def destroy(game_id: String): Unit = {
    val request = createGetRequest(server + "/destroy")
    request.addParameters( Map("id" -> game_id) )
    val result = request.send()
    interpret(result)
  }

  /** Renvoie le nom de l'utilisateur connecte */
  def whoami(): String = {
    val request = createGetRequest(server + "/whoami")
    val result = request.send()
    interpret(result) match {
      case Some(r) =>
        // recupere le dernier mot de la chaine qui correspond au login
        (r \ "status").extract[String].split(' ').last
      case None => ""
    }
  }

  /** Arrete une partie en cours */
  def shutdown(game_id: String): Unit = {
    val request = createGetRequest(server + "/shutdown")
    request.addParameters( Map("id" -> game_id) )
    val result = request.send()
    interpret(result)
  }

  /** Permet d'envoyer des commandes a ses fourmis
    * lors d'une partie en cours.
    * 
    * @TODO: Parsing de la reponse: non fonctionnel
    */
  def play(game_id: String, cmds: List[AntCommand]): Unit = {
    //transforme la liste de AntCommand en String avec des ',' entre les commandes
    val cmdsAsString = cmds map (x => x.toString) reduce ((x,y) => x + "," + y)
    //println("play: " + cmdsAsString)
    val request = createGetRequest(server + "/play")
    request.addParameters( Map("id" -> game_id, "cmds" -> cmdsAsString) )
    val result = request.send()
    interpret(result)
  }

  /** Renvoie l'historique d'une partie.
    * 
    * @TODO: Parsing de la reponse
    */
  def log(game_id: String): String = {
    val request = createGetRequest(server + "/log")
    request.addParameters( Map("id" -> game_id) )
    request.send()
  }

  /* Recupere une chaine de caractere correspondant a du JSON
   * et effectue son parsing.
   * Si la chaine jsonString correspond a une erreur de requete,
   * on affiche l'erreur correspondant.
   * Sinon on renvoie la valeur parsee */
  private def interpret(jsonString: String): Option[JValue] = {
    val json = parse(jsonString)
    val status = (json \ "status").extract[String]
    val response = json \ "response"
    
    status match {
      case "completed" => Some(response)
      case _ => printError(response); None
    }
  }

  private def printError(json: JValue): Unit =
    println(json.extract[RequestError])
}
