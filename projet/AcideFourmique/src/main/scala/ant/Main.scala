package ant

import akka.actor.ActorSystem
import akka.actor.Props

object Main {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("AcideFourmique")
    val a = system.actorOf(Props[IA], "ia")
  }

}