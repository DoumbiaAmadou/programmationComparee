import java.io.OutputStream;
import java.net.Socket;
import java.io.IOException;


object GameLog {

//permet d'envoyer le log d'une partie sous forme d'un string

  private val port = 6666;
  private val ip = "localhost";

  def sendLog(log : String) : Unit = {
    try {
      val s : Socket = new Socket(ip, port);
      val os : OutputStream = s.getOutputStream();
      os.write(log.getBytes());
    } catch {
      case ioe : IOException =>
	println("Connection problem");
    }
  }

}
