package comm.parse

import aliases.Type._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.FieldSerializer._
import comm.DataJson
import world.Rock
import world.Water
import com.sun.xml.internal.ws.developer.Serialization
import world.Grass
import world.Kind

object JSONParse {
  
  /**observation
   * 
   * parse l'observation
   * @json_string : la chaine au format json 
   */
  def observation(json_string:String):Observations={
    implicit val formats = Serialization.formats(NoTypeHints) + new ObservationSerializer
    read[Observations](json_string)//XXX potentielles erreur à catcher
  }
  
  /**msg_server
   * 
   * parse un message du serveur
   * 
   * @json_string : la chaine au format json 
   */
  def msg_server(json_string:String):Response={
      implicit val formats = DefaultFormats
      val p =  net.liftweb.json.parse(json_string)
      val err = p.extractOpt[ErrorResponse]
      if(err.isDefined)
        return err.get
      val empty = p.extractOpt[EmptyResponse]
      if(empty.isDefined)
        return empty.get
        
      throw new Exception("Unparsable json message from server")
      return null//non atteignable
  }
 
  /**msg_create
   * 
   * parse le message d'une nouvelle partie
   * @json_string : la chaine au format json
   */
    def msg_create(json_string:String):NewGameMsg={
      implicit val formats = net.liftweb.json.DefaultFormats
      val p =  net.liftweb.json.parse(json_string)
      p.extract[NewGameMsg]//TODO ne pas faire une conversion brute (utiliser extractOpt)
    }
}

private object Utils{
  
  def makeKind(k:String):Kind={ 
    k match{
    case "grass"  => Grass
    case "rock"   => Rock
    case "water" => Water
    case k        => throw new Exception("Unknown Kind type named: <"+k+">")
    }
  }
  
  def makeBrain(b:String):Brain={
    b match{
      case "controlled" => Controlled
      case "ennemy"     => Ennemy
      case "zombie"     => Zombie
      case b            => throw new Exception("Unknown Brain type named: <"+b+">")
    }
  }
}

/** ObservationSerializer
 *  
 * Cette classe est utilisée par le parseur Json pour 
 * identifier les éléments qui constituent une observation
 * */
class ObservationSerializer extends CustomSerializer[Observations](format => (
    {
       /*observations du jeu*/
       case JObject(  JField("status",JString(status) ) 
                      :: JField("response",JObject(
                          JField("turn",JInt(turn))
                          :: JField("observations",JArray(list_ant:List[JValue]))
                          ::Nil))   
                      :: Nil)=>
         var ant_infos : List[AntInfos]=Nil
         /*list des informations sur les fourmies*/
         for(ant_inf <- list_ant){
           var ant_state : AntState = null
           var ant_view : List[AntView] = Nil
           ant_inf match{
             case JArray(a_l:List[JValue])=>
               for( a <-a_l){
                 a match{
                   /*informations l'état d'une fourmie (position,orientation,vie..)*/
                   case JObject(  JField("x",JInt(x))
                                  ::JField("y",JInt(y))
                                  ::JField("dx",JInt(dx)) 
                                  ::JField("dy",JInt(dy)) 
                                  ::JField("brain",JString(brain))
                                  ::JField("id",JInt(id))
                                  ::JField("energy",JInt(energy))
                                  ::JField("acid",JInt(acid))
                                  ::Nil)=>ant_state = new AntState(  x.intValue,
                                                                     y.intValue,
                                                                     dx.intValue,
                                                                     dy.intValue,
                                                                     Utils.makeBrain(brain),
                                                                     id.intValue,
                                                                     energy.intValue,
                                                                     acid.intValue)
                   /*informations sur l'environnemet d'un fourmi*/
                   case JArray(view:List[JValue])=>
                     for(v <- view){
                       v match{
                         case JObject(  JField("x",JInt(x))::
                                 JField("y",JInt(y))::
                                 JField("content",JObject(List(JField("kind",JString(kind)))))
                                 ::Nil
                              )=>ant_view = ant_view.::(new AntView(x.intValue,y.intValue,List(Utils.makeKind(kind))))
                         case _=>
                       }                           
                     }
                   case _=> 
                 }
               }
             case _=>
           }
           ant_infos = ant_infos.::(new AntInfos(ant_state,ant_view))
         }
         new Observations(status,turn.intValue,ant_infos) 
    },
    {
      case x: Observations =>
        JObject(Nil)
    }
))

/*Response*/
abstract class Response()
case class ErrorResponse(status:String,response:ErrorInfos)extends Response
case class EmptyResponse(status: String, response: NoThing)extends Response
case class NewGameMsg(status:String,response:CreateResponce)extends Response


/*sous champs response*/
case class NoThing()
case class ErrorInfos(error_code:Int,error_msg:String)
case class CreateResponce(identifier:StringId)


/*Observations*/
case class AntView(val x:Int, val y:Int, val content:List[Kind]) 
case class AntState(val x:Int, val y:Int, val dx:Int, val dy:Int, brain:Brain, val id:Int, val energy:Int, val acid:Int)
case class AntInfos(val ant_infos:AntState, ant_view:List[AntView])
case class Observations(val status:String, val turn:Int, val ants_infos:List[AntInfos]) extends Response


/*Brain*/
abstract class Brain(val value:String)
object Controlled extends Brain("controlled")
object Ennemy extends Brain("ennemy")//XXX non vérifié
object Zombie extends Brain("zombie")//XXX non vérifié


object TestJSONParse{
  def main(args: Array[String])={
    
    val json = DataJson.play_lighter
    val o = JSONParse.observation(json)
    println("OBSERVATION : "+o)
    
    val err = DataJson.auth_err
    val e = JSONParse.msg_server(err)
    println("ERROR MSG : "+e)
    
    val empty = DataJson.auth_ok
    val en = JSONParse.msg_server(empty)
    println("EMPTY MSG : "+en)
  }
}