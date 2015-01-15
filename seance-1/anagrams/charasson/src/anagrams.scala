import scala.io.Source
import scala.collection.mutable._
import collection.mutable
import scala.collection.SortedMap

object anagrams {
  val WORDS_FILE = "../words"
  def main(args: Array[String]) {
    val word_list = Source.fromFile(WORDS_FILE).getLines().toList
    val mm = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
    for(w<-word_list){
      var wo = w
      var key = wo.toCharArray().sorted.mkString("")
      mm.addBinding(key, w)
    }
    //On tri les mots dans l'ordre alphabétique 
    var sorted_word = SortedMap[String,String]()
    for(arg<-args){
    	var wo = arg
      var key = wo.toCharArray().sorted.mkString("")
      var rep = mm.find(p => p._1 == key)
      var str = ""
    	if(rep.isDefined){
    		for(r<-rep){
    			for (rr<-r._2)
    				str += " "+rr
    		}
    	}
    	sorted_word+=(arg->str)
    }
    //affichage sur la sortie standard:
    for(sw<-sorted_word){
      println(sw._1+" : "+sw._2)
    }
  }
}
