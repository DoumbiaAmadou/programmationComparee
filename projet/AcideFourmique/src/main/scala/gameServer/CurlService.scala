package gameServer

import sys.process._

/** Methode de communication avec un serveur en utilisant
  * le programme curl. Implemente la facade HttpService.
  * Pour changer de methode de communication, il suffit d'implementer
  * le trait HttpService. */
object CurlService extends HttpService {
  type Host = String
  type Response = String

  def initServer(url: String): Host = url

  def createGetRequest(server: Host): Request =
    new CurlRequest("-G " + server)

  def createPostRequest(server: Host): Request =
    new CurlRequest(server + " -X POST")


  private class CurlRequest(svc: Host) extends Request {
    private var _service =
      "curl -s -k --cookie " + cookiePath +
        " --cookie-jar " + cookiePath + " " + svc
 
    def send(): Response = {
      val stream = Process(_service).lines_!
      stream reduce ((x,y) => x + y)
    }

    def addParameters(params: Map[String,String]): Unit = {
      params foreach { case (k,v) => k match {
        case "teaser" =>
          _service = _service + " --data-urlencode teaser=" + v
        case _ => _service = _service + " -d " + k + "=" + v
      }}
    }
  }

}
