package comm

import game.Game
import scala.collection.immutable._
import aliases.Type._;

class Parse(val game:Game){
  private var actions:SortedMap[AntNum,Command] = new TreeMap
  private val network = new Network
  
  /**parse l'action sur la forme "<chiffre>:<command>"*/
  private def parse_action(ant_num:AntNum, command:Command):String=
    ant_num.toString+":" + command.parsed
  
  /**envoyer toutes les actions*/
  def play_actions()={
    val cmds:String = actions.foldLeft("")((acc,kv)=>acc+parse_action(kv._1,kv._2)+"; ")
    println("CHAINE : "+cmds)//TODO test à retirer
    //network.play(game.id, cmds)//TODO à remettre
  }
  
  /**Ajoute une action pour une fourmie*/
  def add_action(ant_num:Int,command:Command)=
    actions = actions + ((ant_num,command))

  def 



}





object Parse{
  def main(args: Array[String])={
    val game = new Game("","",0,0,0,0,0,0,0)
    val parse = new Parse(game)
    parse.add_action(0, Atk(20))
    parse.add_action(2, Left)
    parse.add_action(1, Right)
    //parse.add_action(7, Hack(Rest))
    //parse.add_action(5, Hack(Store("x",N(12))))
    val l = List(  (None,Rest),
                   (Some("BB"),Store("x",N(44))),
                   (Some("AA"),Left),
                   (None,Right),
                   (None,Store("y",Add(1,2)))
                )
    parse.add_action(10, Hack(l))
    parse.sent_actions()
  }
}