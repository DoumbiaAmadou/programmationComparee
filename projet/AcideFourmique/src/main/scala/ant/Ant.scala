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
	  case Ant.AttackAction => {
	    println("Attack")
	    //Attack(10);
	    sender() ! Ant.Done
	  }
	  case Ant.Move   => {
		println("Move")
		//Forward()
		sender() ! Ant.Done
	  }
	  case Ant.Rotate => {
	    println("Rotate")
	    //val rotate = List(Right(), Left())
	    //rotate(Random.nextInt(rotate.length))
	    sender() ! Ant.Done
	  }
	  case Ant.Stand  => {
	    println("Stand")
	    //Rest()
	    sender() ! Ant.Done
	  }
	  case Ant.Hack => {
	    println("Hack")
	    sender() ! Ant.Done
	  }
	}
  
}