import scala.io.Source
import scala.collection.mutable._
import collection.mutable
import scala.collection.SortedMap

object anagram {
  val WORDS_FILE = "../../words"
  def main(args: Array[String]) {
    //val word_list = Source.fromFile(WORDS_FILE).getLines().toList //ancienne version
    val word_list = args
    val mm = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
    for(w<-word_list){
      var wo = w
      wo = "é|è|ê|ë".r.replaceAllIn(wo, "e")
      wo = "à|â|ä".r.replaceAllIn(wo, "a")
      wo = "ô|ö".r.replaceAllIn(wo, "o")
      wo = "î|ï".r.replaceAllIn(wo, "i")
      wo = "ù|û|ü".r.replaceAllIn(wo, "u")
      wo = "-|_".r.replaceAllIn(wo, "")
      var key = wo.toCharArray().sorted.mkString("")
      mm.addBinding(key, w)
    }
    //On tri les mots dans l'ordre alphabétique 
    var sorted_word = SortedMap[String,String]()
    for(a<-mm){
      for(aa<-a._2){
        if(a._2.size==1){
          sorted_word+=(aa->"_")
        }
        else{
          var str = ""
          for(bb<-a._2){
            if(!bb.equals(aa)){
              str+= bb+" "
            }
          }
          sorted_word+=(aa->str)
        }
      }
    }
    //affichage sur la sortie standard:
    for(sw<-sorted_word){
      println(sw._1+" : "+sw._2)
    }
  }
}
