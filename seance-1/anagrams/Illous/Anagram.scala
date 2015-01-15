import scala.io.Source
import scala.collection.immutable.TreeMap

object Anagram {

  def isAnagram(w : String, u : String) : Boolean = {
    if(w.length() != u.length()) {
      return false
    }
    occurences(w)== occurences(u);
  }
  
  def occurences(w : String) : TreeMap[Char,Int]= {
    var occurence = new TreeMap[Char,Int];//pour chaque lettre associe son nombre d'occurence
    for(l <- w) {
      occurence.get(l) match {
        case None => {occurence += ((l,1))}
        case Some(i) => {occurence += ((l,i+1))}
      }
    }
    occurence
  }
  
  def anagram(d : List[String], w : String) : Unit = {
    for(wi <- d) {
      if(isAnagram(w,wi)) {
        print(wi + " ")
      }
    }
  }
  
  def main(args : Array[String]) : Unit = {
    args.length match {
      case 0 => {
    	  println("Veuillez ecrire au moins un mot en argument")
      }
      case _ => {
        val dico = Source.fromFile("../words").getLines.toList;
        for(w <- args) {
        	print(w + ": ");
        	anagram(dico,w);
        	println();
        }
      }
    }
  }
  
  
}
