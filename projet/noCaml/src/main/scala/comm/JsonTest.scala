package comm

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.FieldSerializer._

/*
//trait Response
abstract class Response()
case class ErrorResponse(error_code:Int,error_msg:String)extends Response
case class EmptyResponse()extends Response

case class ParseJson(status:String,response:ErrorResponse)


class ResponseSerializer extends CustomSerializer[ErrorResponse](format => (
    {
      case JObject(JField("status", JString("error"))::JField("response", JObject(JField("error_code",JInt(errc)) :: JField("error_msg",JString(errm)):: Nil)) :: Nil) =>
        new ErrorResponse(errc.intValue,errm.toString)
      case JObject(List(JField("status",JString(status)), JField("response",JObject(a:List[JValue]))))=>
        new ErrorResponse(0,"no error")

    },
    {
      case x: ErrorResponse =>
        JObject(JField("status", JString("error"))::JField("response", JObject(JField("error_code",JInt(x.error_code)) :: JField("error_msg",JString(x.error_msg)):: Nil)) :: Nil)
    }
 ))

case class Kind(val kind:String)
case class AntView(val x:Int, val y:Int, val content:List[Kind]) 
case class AntState(val x:Int, val y:Int, val dx:Int, val dy:Int, brain:String, val id:Int, val energy:Int, val acid:Int)//TODO remplacer le type de brain par un type enum Brain mm chose pour status
case class AntInfos(val ant_infos:AntState, ant_view:List[AntView])
case class Observations(val status:String, val turn:Int, val ants_infos:List[AntInfos])

object JsonTest {
  def main(args: Array[String])={
    val foo = Foo()
    foo.bar
  }
}

/*divers tests*/
case class Foo(){
	
  
    def bar()={
    //implicit val formats = net.liftweb.json.DefaultFormats
    implicit val formats = Serialization.formats(NoTypeHints) + new ResponseSerializer
    val data = DataJson
    
    val json1 = net.liftweb.json.parse(data.auth_ok)    
    println("#1#"+json1 +"\n\n")
    val json2 = net.liftweb.json.parse(data.auth_err)    
    println("#2#"+json2)
    println("#1#"+json1.extractOpt[ParseJson])
    println("#2#"+json2.extractOpt[ParseJson])
    
    println("#1A#"+ read[ErrorResponse](data.auth_ok))
    println("#2B#"+ read[ErrorResponse](data.auth_err))

  }
    
  def baz={
    implicit val formats = Serialization.formats(NoTypeHints) + new ResponseSerializer
    val data = DataJson
    val json = net.liftweb.json.parse(data.auth_err)
    println(json)
  }
  

    def foo={
    
    class MySerializer extends CustomSerializer[Observations](format => (
        {
           /*observations du jeu*/
           case JObject(  JField("status",JString(status) ) 
                          :: JField("response",JObject(
                              JField("turn",JInt(turn))
                              :: JField("observations",JArray(list_ant:List[JValue]))
                              ::Nil))   
                          :: Nil)=>
             var ant_infos : List[AntInfos]=Nil
             println("\n\n")
             /*list des informations sur les fourmies*/
             for(ant_inf <- list_ant){
               var ant_state : AntState = null
               var ant_view : List[AntView] = Nil
               println("Ant Info:")
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
                                      ::Nil)=> println("id["+id+"] coord["+x+";"+y+"]")
                                               ant_state = new AntState(  x.intValue,
                                                                          y.intValue,
                                                                          dx.intValue,
                                                                          dy.intValue,
                                                                          brain,
                                                                          id.intValue,
                                                                          energy.intValue,
                                                                          acid.intValue)
                       /*informations sur l'environnemet d'un fourmie*/
                       case JArray(view:List[JValue])=>
                         for(v <- view){
                           v match{
                             case JObject(  JField("x",JInt(x))::
                                     JField("y",JInt(y))::
                                     JField("content",JObject(List(JField("kind",JString(kind)))))
                                     ::Nil
                                  )=>println("["+x+";"+y+"]"+kind)
                                     ant_view = ant_view.::(new AntView(x.intValue,y.intValue,List(Kind(kind))))
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

    implicit val formats = Serialization.formats(NoTypeHints) + new MySerializer

    val json = net.liftweb.json.parse(DataJson.play)
    println(json)
    val obs:Observations = read[Observations](DataJson.play) 
    println("\n\n"+obs.turn)

  }
}






/*exemples de données reçu après une requête*/
object DataJson {
    val register = """{ "status": "completed", "response": {} }"""
    val auth_err ="""{
                        "status": "error",
                        "response": {
                          "error_code": 202165063,
                          "error_msg": "I do not know you, aoaoao."
                        }
                     }"""
    
   val auth_ok = """{ "status": "completed", "response": {} }"""
    
    val status = """{
                      "status": "error",
                      "response": {
                        "error_code": 796193025,
                        "error_msg": "Invalid game identifier 11"
                      }
                    }"""
    
    val whoami ="""{ "status": "completed", "response": { "status": "logged as zzz" } }"""
    

    val play =
      """
{
  "status": "completed",
  "response": {
    "turn": 57,
    "observations": [
      [
        {
          "x": 43,
          "y": 90,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 4,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 42, "y": 89, "content": { "kind": "grass" } },
          { "x": 43, "y": 89, "content": { "kind": "grass" } },
          { "x": 44, "y": 89, "content": { "kind": "grass" } },
          { "x": 42, "y": 90, "content": { "kind": "grass" } },
          { "x": 43, "y": 90, "content": { "kind": "grass" } },
          { "x": 44, "y": 90, "content": { "kind": "grass" } },
          { "x": 42, "y": 91, "content": { "kind": "grass" } },
          { "x": 43, "y": 91, "content": { "kind": "grass" } },
          { "x": 44, "y": 91, "content": { "kind": "grass" } }
        ],
        [ { "x": 43, "y": 90, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 23,
          "y": 68,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 3,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 22, "y": 67, "content": { "kind": "grass" } },
          { "x": 23, "y": 67, "content": { "kind": "grass" } },
          { "x": 24, "y": 67, "content": { "kind": "grass" } },
          { "x": 22, "y": 68, "content": { "kind": "grass" } },
          { "x": 23, "y": 68, "content": { "kind": "grass" } },
          { "x": 24, "y": 68, "content": { "kind": "grass" } },
          { "x": 22, "y": 69, "content": { "kind": "grass" } },
          { "x": 23, "y": 69, "content": { "kind": "grass" } },
          { "x": 24, "y": 69, "content": { "kind": "grass" } }
        ],
        [ { "x": 23, "y": 68, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 3,
          "y": 269,
          "dx": 0,
          "dy": 1,
          "brain": "controlled",
          "id": 2,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 2, "y": 268, "content": { "kind": "grass" } },
          { "x": 3, "y": 268, "content": { "kind": "grass" } },
          { "x": 4, "y": 268, "content": { "kind": "grass" } },
          { "x": 2, "y": 269, "content": { "kind": "grass" } },
          { "x": 3, "y": 269, "content": { "kind": "grass" } },
          { "x": 4, "y": 269, "content": { "kind": "grass" } },
          { "x": 2, "y": 270, "content": { "kind": "grass" } },
          { "x": 3, "y": 270, "content": { "kind": "grass" } },
          { "x": 4, "y": 270, "content": { "kind": "grass" } }
        ],
        [ { "x": 3, "y": 269, "dx": 0, "dy": 1, "brain": "controlled" } ]
      ],
      [
        {
          "x": 82,
          "y": 258,
          "dx": -1,
          "dy": 0,
          "brain": "controlled",
          "id": 1,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 81, "y": 257, "content": { "kind": "grass" } },
          { "x": 82, "y": 257, "content": { "kind": "grass" } },
          { "x": 83, "y": 257, "content": { "kind": "grass" } },
          { "x": 81, "y": 258, "content": { "kind": "grass" } },
          { "x": 82, "y": 258, "content": { "kind": "grass" } },
          { "x": 83, "y": 258, "content": { "kind": "grass" } },
          { "x": 81, "y": 259, "content": { "kind": "grass" } },
          { "x": 82, "y": 259, "content": { "kind": "grass" } },
          { "x": 83, "y": 259, "content": { "kind": "grass" } }
        ],
        [ { "x": 82, "y": 258, "dx": -1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 55,
          "y": 71,
          "dx": -1,
          "dy": 0,
          "brain": "controlled",
          "id": 0,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 54, "y": 70, "content": { "kind": "grass" } },
          { "x": 55, "y": 70, "content": { "kind": "grass" } },
          { "x": 56, "y": 70, "content": { "kind": "grass" } },
          { "x": 54, "y": 71, "content": { "kind": "grass" } },
          { "x": 55, "y": 71, "content": { "kind": "grass" } },
          { "x": 56, "y": 71, "content": { "kind": "grass" } },
          { "x": 54, "y": 72, "content": { "kind": "grass" } },
          { "x": 55, "y": 72, "content": { "kind": "grass" } },
          { "x": 56, "y": 72, "content": { "kind": "grass" } }
        ],
        [ { "x": 55, "y": 71, "dx": -1, "dy": 0, "brain": "controlled" } ]
      ]
    ]
  }
}
      """
    val play_lighter =
      """
{
  "status": "completed",
  "response": {
    "turn": 57,
    "observations": [
      [
        {
          "x": 43,
          "y": 90,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 4,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 42, "y": 89, "content": { "kind": "grass" } },
          { "x": 43, "y": 89, "content": { "kind": "grass" } }
        ],
        [ { "x": 43, "y": 90, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 23,
          "y": 68,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 3,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 22, "y": 67, "content": { "kind": "grass" } },
          { "x": 23, "y": 67, "content": { "kind": "grass" } }
        ],
        [ { "x": 23, "y": 68, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ]
    ]
  }
}
      """
    

}
*/