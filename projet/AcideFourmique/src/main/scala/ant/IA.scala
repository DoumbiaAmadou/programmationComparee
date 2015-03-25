package ant

import akka.actor.Actor
import akka.actor.Props

class IA extends Actor {

  override def preStart() {
    val ant = context.actorOf(Props[Ant], "ant")
    val ant2 = context.actorOf(Props[Ant], "ant2")
    ant ! Ant.AttackAction
    ant2 ! Ant.Move

  }
  
  def receive = {
  	case Ant.Done => println(sender() + "Action Done")
  }
  
}