package ant

import akka.actor.Actor
import akka.actor.Props

object IA {
   case object Exit
}

class IA extends Actor {

  override def preStart() {
    //Two ant are created
    val ant = context.actorOf(Props[Ant], "ant")
    val ant2 = context.actorOf(Props[Ant], "ant2")
    
    /* Ask one of the ants to attack and ask the other
     * one to move forward
     */
    ant ! Ant.AttackAction
    ant2 ! Ant.Move
  }
  
  def receive = {
    case Ant.Done => println("Done")
  }
  
}
