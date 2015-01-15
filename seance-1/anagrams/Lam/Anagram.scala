import scala.collection.mutable.ListMap
import scala.io.Source

object Anagram {
	def computeOcc(word : String) : ListMap[Char, Int] = {
		val listOccurence = new ListMap[Char, Int]
		var occ = 1
		
		for(i <- 0 to word.length() - 1){
			if(listOccurence.contains(word.charAt(i))) {
				listOccurence.put(word.charAt(i), occ + 1)
			} else {
				listOccurence.put(word.charAt(i), occ)		      
			}
		}  
		return listOccurence
	}
	
	def isAnagram(word1 : String, word2 : String) : Boolean = {
		val w1 = word1.toLowerCase()
		val w2 = word2.toLowerCase()
			
		if(w1.length() != w2.length() || w1.equals(w2)) {
			return false
		} else {
			val occ1 = computeOcc(w1)
			val occ2 = computeOcc(w2)
			return (occ1 == occ2)
		}
	}
	
	def anagram(word : String, dictionary : List[String]) {
	  dictionary match {
	  case List() => {
		  println(word + ": ")
	  }
	  case h::t => {
		  if(isAnagram(word, h)) {
			  println(word + ": " + h)
		  }
		  anagram(word, t)
	  }
	  }
	}
  
	def main(args : Array[String]) : Unit = {
		//val FILE = "/usr/share/dict/words"
		val FILE = Source.fromFile("../words")
		val tmp = FILE.getLines.toList

		for(word <- args) {
//		  println(word + ": ici")
		  anagram(word, tmp)
		}
	}
	
}