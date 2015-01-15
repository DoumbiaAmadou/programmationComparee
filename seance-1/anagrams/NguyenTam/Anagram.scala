import scala.collection.mutable.ListBuffer
import scala.io.Source

object Anagrames {
  def trouveAnagram(str: String, motListe: ListBuffer[String]): ListBuffer[String] = {
    var anagrams: ListBuffer[String] = ListBuffer.empty
    val sortedStr = str.trim().toCharArray().sorted.mkString("")
    for (mot <- motListe){
      val sortedMot = mot.trim().toCharArray().sorted.mkString("")
      if (sortedStr.equals(sortedMot)){
        anagrams.+=(mot)
      }
    }
    anagrams
  }

  def main(args: Array[String]): Unit = {
    val dictionary : ListBuffer[String]= Source.fromFile("src/words").getLines().toList.to[ListBuffer]
    for(arg <- args) {
      println(arg + " : "+ trouveAnagram(arg, dictionary))
    }
  }
}