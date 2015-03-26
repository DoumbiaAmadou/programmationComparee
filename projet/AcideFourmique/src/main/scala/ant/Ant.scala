package ant

import akka.actor.Actor

object Ant {
  case object AttackAction 
  case object Move 
  case object Rotate 
  case object Stand
  case object Hack
  case object Done
}

class Ant extends Actor {

	def receive = {
	  //The ant have received the message to attack
	  case Ant.AttackAction => {
	    println("Attack")
	    sender() ! Ant.Done
	    context.stop(self)
	  }
	  
	  //The ant have received the message to move forward	  
	  case Ant.Move   => {
	    println("Move")
	    sender() ! Ant.Done
	    context.stop(self)
	  }
	  
	  //The ant have received the message to rotate
	  case Ant.Rotate => {
	    println("Rotate")
	    sender() ! Ant.Done
	    context.stop(self)
	  }
	  
	  //The ant have received the message to stand still
	  case Ant.Stand  => {
	    println("Stand")
	    sender() ! Ant.Done
	    context.stop(self)
	  }
	  
	  //The ant have received the message to hack a dead ant
	  case Ant.Hack => {
	    println("Hack")
	    sender() ! Ant.Done
	    context.stop(self)
	  }
	}
  
}
