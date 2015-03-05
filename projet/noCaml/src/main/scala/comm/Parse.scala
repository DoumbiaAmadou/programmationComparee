package comm

import game.Game
import scala.collection.immutable._
import org.omg.CORBA.Object
import game.Game

class Parse(val game:Game){
  /*Int correspond au numéros de la fourmie et Command à la commande*/
  private var actions:SortedMap[Int,Command] = new TreeMap
  private val network = new Network
  
  /*parse l'action sur la forme "<chiffre>:<command>"*/
  private def parse_action(ant_num:Int,command:Command):String=
    ant_num.toString+":" + command.parsed
  
  /*envoyer toutes les actions*/
  def sent_actions()={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parse_action(kv._1,kv._2)+"; ")
    println("voici la chaine parse: "+cmds)//TODO test à retirer
    //network.play(game.id, cmds)//TODO à remettre
  }
  
  /*Ajoute une action pour une fourmie*/
  def add_action(ant_num:Int,command:Command)=
    actions = actions + ((ant_num,command))
}

object Parse{
  def main(args: Array[String])={
    val game = new Game("","",0,0,0,0,0,0,0)
    val parse = new Parse(game)
    parse.add_action(0, Atk(20))
    parse.add_action(2, Left)
    parse.add_action(1, Right)
    parse.add_action(7, Hack(Rest))
    parse.add_action(5, Hack(Store("x",N(12))))
    val l = new CodeList(List(Rest,Store("x",N(44)),Left,Right))
    parse.add_action(10, Hack(l))
    parse.sent_actions()
  }
}