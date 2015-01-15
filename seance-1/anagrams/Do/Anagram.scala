import scala.collection.mutable.ListMap
import scala.io.Source

object Main {

  def occurrences(word: String): ListMap[Char,Int] = {
    val occur = new ListMap[Char,Int]
	for (c <- word) {
		if (occur.contains(c)) {
			occur.put(c,
			    occur.get(c) match {
					case None => 1 //cas impossible
					case Some(i) => i+1
				}
			)
		} else occur.put(c, 1)
	}
    occur
  }
  
  def anagram(w: String, a: String): Boolean = {
    val lowerW = w.toLowerCase()
    val lowerA = a.toLowerCase()
    if (w.length() != a.length()) false
    else {
      val wOccur = occurrences(lowerW)
      val aOccur = occurrences(lowerA)
      wOccur.equals(aOccur)
    }
  }
  
  def anagrams(w: String, list: List[String]): List[String] = {
    list match {
      case List() => println(); List()
      case h::t => {
        if (anagram(w,h)) print(h+" ")
        anagrams(w,t)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {  
	val file = Source.fromFile("../words")
	val dico = file.getLines.toList
    	
	for (word <- args) {
	  print(word+": ")
	  anagrams(word, dico)
	}
  }
}
