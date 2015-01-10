import scala.io.Source

object anagrams {

  def findAnagrams(word : String, wordList : List[String]) : String = {
    var anagrams = ""
    val sortedWord = word.toCharArray().sorted.mkString("")
    for(dictWord <- wordList) {
      if(dictWord != word) {
	val sortedDictWord = dictWord.toCharArray().sorted.mkString("")
	if(sortedWord == sortedDictWord)
	  anagrams += " "+ dictWord
      }
    }
    anagrams
  }

  def main(args: Array[String]) = {
    val dictWords = Source.fromFile("../words").getLines().toList

    for(arg <- args) {
      println(arg+" : "+findAnagrams(arg, dictWords))
    }
  }

}
