package comm.parse

import aliases.Type._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.FieldSerializer._
import comm.DataJson

/**
 * JSJONParse contient toute méthodes nécéssaire pour parser les messages du
 * serveur (qui sont au format json). Ce code est utilisé uniquement par Parse. 
 * Le Parseur utilisé est liftweb (package json).
 * 
 * Note : Il faudrait mettre certaines partie du code dans un try catch.
 *        Car, il y a potentiellement une exception qui sera lancé si la
 *        chaine de caractère json n'est pas reconnu au correctement
 *        parsée.
 */
object JSONParse {
  
  /** parse l'observation
   *  @param json_string est la chaine au format json 
   */
  def observation(json_string:String):Observations={
    implicit val formats = Serialization.formats(NoTypeHints) + new ObservationSerializer
    read[Observations](json_string)//TODO potentielles erreur à catcher
  }
  
  /** parse un message du serveur
   *  @param json_string est la chaine au format json 
   */
  def msgServer(json_string:String):Response={
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
 
  /** parse le message d'une nouvelle partie
   *  @parma json_string est la chaine au format json
   */
    def msgCreate(json_string:String):NewGameMsg={
      implicit val formats = net.liftweb.json.DefaultFormats
      val p =  net.liftweb.json.parse(json_string)
      p.extract[NewGameMsg]//TODO ne pas faire une conversion brute (utiliser extractOpt)
    }
}


/** Cette classe est utilisée par le parseur Json pour 
 *  identifier les éléments qui constituent une observation.
 *  Et ainsi les rendre utilisable dans le code.
 *  
 *  Note : La règle des 80 caractère rend plus difficile la lecture du code
 *         car il y a un nombre important d'imbrications dans la chaine json
 *         à parcer. La raison est aussi que le parseur liftweb n'est pas
 *         approprier pour ce genre de chaines json.
 */
class ObservationSerializer extends CustomSerializer[Observations](format => (
    {
       /*observations du jeu*/
       case JObject(  JField("status",JString(status) ) 
                      :: JField("response",JObject(
                          JField("turn",JInt(turn))
                          ::JField("observations",
                              JArray(list_ant:List[JValue]))
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
                   /*informations fourmie (position,orientation,vie..)*/
                   case JObject(  
                       JField("x",JInt(x))
                       ::JField("y",JInt(y))
                       ::JField("dx",JInt(dx)) 
                       ::JField("dy",JInt(dy)) 
                       ::JField("brain",JString(brain))
                       ::JField("id",JInt(id))
                       ::JField("energy",JInt(energy))
                       ::JField("acid",JInt(acid))
                       ::Nil)=>ant_state = new AntState(x.intValue,
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
                         case JObject(
                             JField("x",JInt(x))
                             ::JField("y",JInt(y))
                             ::JField("content",
                                 JObject(List(JField("kind",JString(kind)))))
                             ::Nil)=>
                             ant_view = ant_view.::(
                                 new AntView(  x.intValue,
                                               y.intValue,
                                               List(Utils.makeKind(kind))))
                         case _=>/*ignoré*/
                       }                           
                     }
                   case _=>/*ignoré*/
                 }
               }
             case _=>/*ignoré*/
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

/** Les fonctions de Utils sont utilisées par ObservationSerializer
 *  pour allèger son code
 */
private object Utils{
  
  /** retourne le Kind correspondant à "k"*/
  def makeKind(k:String):Kind={ 
    k match{
    case "grass"  => Grass
    case "rock"   => Rock
    case "water"  => Water
    case "mill"   => Food //XXX non vérifier
    case k        => throw new Exception("Unknown Kind type named: <"+k+">")
    }
  }
  
  /** retourne le Brian correspondant à "b"*/
  def makeBrain(b:String):Brain={
    b match{
      case "controlled" => Controlled
      case "ennemy"     => Ennemy
      case "zombie"     => Zombie
      case b            => throw new Exception("Unknown Brain type named: <"+b+">")
    }
  }
}


/**Response*/
abstract class Response()
case class ErrorResponse(status:String,response:ErrorInfos)extends Response
case class EmptyResponse(status: String, response: NoThing)extends Response
case class NewGameMsg(status:String,response:CreateResponce)extends Response


/**sous champs response*/
case class NoThing()
case class ErrorInfos(error_code:Int,error_msg:String)
case class CreateResponce(identifier:StringId)


/**Observations*/
case class AntView(val x:Int, val y:Int, val content:List[Kind]) 
case class AntState(val x:Int, val y:Int, val dx:Int, val dy:Int, brain:Brain, val id:Int, val energy:Int, val acid:Int)
case class AntInfos(val ant_infos:AntState, ant_view:List[AntView])
case class Observations(val status:String, val turn:Int, val ants_infos:List[AntInfos]) extends Response

/**Kind*/

abstract class Kind(val value:String)
object Grass extends Kind("grass")//verifié
object Rock extends Kind("rock")//verifié
object Water extends Kind("water")//verifié
object Food extends Kind("food")//non vérifier

/**Brain*/
abstract class Brain(val value:String)
object Controlled extends Brain("controlled")
object Ennemy extends Brain("ennemy")//XXX non vérifié
object Zombie extends Brain("zombie")//XXX non vérifié


/** Test de JSONParse*/
object TestJSONParse{
  def main(args: Array[String])={
    
    val json = DataJson.play_lighter
    val o = JSONParse.observation(json)
    println("OBSERVATION : "+o)
    
    val err = DataJson.auth_err
    val e = JSONParse.msgServer(err)
    println("ERROR MSG : "+e)
    
    val empty = DataJson.auth_ok
    val en = JSONParse.msgServer(empty)
    println("EMPTY MSG : "+en)
  }
}