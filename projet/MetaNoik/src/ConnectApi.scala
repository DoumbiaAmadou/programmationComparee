import scalaj.http._

object ConnectApi {
	private val baseUrl = "http://yann.regis-gianas.org/antroid/"
	private val version = "0"

	// Send request by method POST
	def requestPOST(requestName: String, params: Seq[(String, String)]) : String = {
		val request: HttpRequest = Http(baseUrl+version+"/"+ requestName +"?")
      		val response: HttpResponse[String] = request.postForm(params).asString
      		return response.body
	}

	// Send request by method GET
	def requestGET(requestName: String, params: ((String,String))) : String = {
		val request: HttpRequest = Http(baseUrl+version+"/"+ requestName +"?")
		println(request)
      		val response: HttpResponse[String] = request.params(params).asString
      		return response.body
	}

	// Connect a user
	def auth(login: String, password: String) : String ={
		var params = Seq("login" -> login, "password" -> password)
		println(params)
		return requestPOST("auth",params)
	}

	// Create a new game. -- probleme: can't send request with params int
	//	def newGame(users: String, teaser: String, pace: String , nb_turn: Int, nb_ant_per_player: Int, nb_player: Int,
		//			 minimal_nb_player: Int, initial_energy: Int, initial_acid: Int) : String = {
		//	val request: HttpRequest = Http(baseUrl+version+"/create?")
		//	var postform = Seq("users"->users,"teaser"->teaser,"pace"->pace, "nb_turn"->nb_turn, 
		//						"nb_ant_per_player"->nb_ant_per_player, "nb_player"->nb_player, 
		//						"minimal_nb_player"->minimal_nb_player, "initial_energy"->initial_energy,
		//						"initial_acid"->initial_acid)
      	//	val response: HttpResponse[String] = request.params(postform).asString
      	//	return response.body
	//	}

	// Destroy a game.
	//	def destroyGame(idGame: String) : String = {
	//		var params =(("id", idGame))
	//		println(params)
	//		return requestGET("destroy",params)
	//	}

	// List all visible games.
	//	def listGame(): String = {
	//		return requestGET("game", null)
	//	}


	
	// Test connect
	def main(args: Array[String]) {
      	    println("Hello, scala request!")
	    println(auth("nga","ngango"))
		// println(newGame("nga", "ngo", 10,30,1,1,1,100,100))
		// println(destroyGame("1"))
		// println(requestGET("auth",))

    }

}
