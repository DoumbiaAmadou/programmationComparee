package server

/** Facade permettant de communiquer avec le serveur.
  * Pour implementer une nouvelle methode de communication, il suffit
  * de creer une classe qui etend ce trait et une sous-classe qui
  * etend le trait Request. */
trait HttpService {
  type Host
  type Response

  private var _cookiePath = ".cookies"
  def cookiePath: String = _cookiePath
  def cookiePath_=(path: String): Unit = _cookiePath = path

  def initServer(url: String): Host
  def createGetRequest(server: Host): Request
  def createPostRequest(server: Host): Request

  trait Request {
    def send(): Response
    def addParameters(params: Map[String,String]): Unit
  }
}
