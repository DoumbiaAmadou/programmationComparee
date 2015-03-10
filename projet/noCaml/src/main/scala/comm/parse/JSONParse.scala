package comm.parse

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.FieldSerializer._


object JSONParse {//TODO pas de gestion d'erreur dans le cas ou la chaine est incorrecte
  
  def observation(json_string:String):Observations={
    implicit val formats = Serialization.formats(NoTypeHints) + new ObservationSerializer
    read[Observations](json_string)//XXX potentielles erreur à catcher
  }
  
  def msg_serveur(json_string:String):ErrorResponse={
    implicit val formats = Serialization.formats(NoTypeHints) + new ResponseSerializer
    read[ErrorResponse](json_string)//XXX potentielles erreur à catcher
  }
 

  def msg_create(json_string:String):NewGameMsg={
    implicit val formats = net.liftweb.json.DefaultFormats
    val p =  net.liftweb.json.parse(json_string)
    p.extract[NewGameMsg]
  }
}

class ResponseSerializer extends CustomSerializer[ErrorResponse](format => (
    {
      case JObject(  JField("status", JString("error"))
                     ::JField("response", JObject(
                         JField("error_code",JInt(errc))
                         :: JField("error_msg",JString(errm))
                         :: Nil))
                     :: Nil) => new ErrorResponse(errc.intValue,errm.toString)
                   
      case JObject(List(JField("status",JString(status)), JField("response",JObject(a:List[JValue]))))=>
        new ErrorResponse(0,"no error")

    },
    {
      case x: ErrorResponse =>
        JObject(  JField("status", JString("error"))
                  ::JField(  "response", JObject(JField("error_code",JInt(x.error_code))
                             :: JField("error_msg",JString(x.error_msg))
                             :: Nil))
                  :: Nil)
    }
))

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
                                                                     brain,
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
                              )=>ant_view = ant_view.::(new AntView(x.intValue,y.intValue,List(Kind(kind))))
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
case class ErrorResponse(error_code:Int,error_msg:String)extends Response
case class EmptyResponse()extends Response//sert à rien
case class CreateResponce(identifier:String)extends Response

/*NewGameMsg*/
case class NewGameMsg(status:String,response:CreateResponce)

/*Observations*/
case class Kind(val kind:String)
case class AntView(val x:Int, val y:Int, val content:List[Kind]) 
case class AntState(val x:Int, val y:Int, val dx:Int, val dy:Int, brain:String, val id:Int, val energy:Int, val acid:Int)//TODO remplacer le type de brain par un type enum Brain mm chose pour status
case class AntInfos(val ant_infos:AntState, ant_view:List[AntView])
case class Observations(val status:String, val turn:Int, val ants_infos:List[AntInfos])