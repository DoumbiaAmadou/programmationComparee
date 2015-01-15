import scala.io.Source.fromFile
import java.io.FileNotFoundException
import scala.io.BufferedSource

object Anagram {

	def getOccurence(lettre : Char, mot : String) : Int = {
		var cpt : Int = 0
		for(i <- 0 to mot.length()-1){
			if(lettre == mot.charAt(i)) cpt += 1
		}
      	return cpt
	}

	def isAnagram(mot1 : String, mot2 : String) : Boolean = {
		if(mot1.equals(mot2) || mot1.length() != mot2.length()
		    || mot1.length() == 0 || mot2.length() == 0) return false
		var lettre : Char = '0'
		for(i <- 0 to mot1.length()-1){
			lettre = mot1.charAt(i)
			if(!mot2.contains(lettre)) return false
			if(getOccurence(lettre,mot1) != getOccurence(lettre,mot2)) return false
		}
		return true
	}

	def getAnagram(mot : String) : Unit = {
		try{
			var file : BufferedSource = fromFile("words")
			val iterator : Iterator[String] =  file.getLines
			var motCourant : String = ""
			  
			print("\n"+mot+" :")
			while(iterator.hasNext){
				motCourant = iterator.next
				if(isAnagram(mot,motCourant))  print(" "+motCourant)
			}
			print("\n");
	  	}catch{
	  		case e:FileNotFoundException => println("dictionnaire introuvable")
	  	}		  
	}

	def main(args : Array[String]) : Unit = {
	    for(mot <- args) getAnagram(mot)
		println("fin")
	}

}