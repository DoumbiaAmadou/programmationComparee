import scala.io.Source

object Anagram {  

  val DICT_FILE = "../words"

  def main(args: Array[String]) {    
    val dict = Source.fromFile(DICT_FILE).getLines().toList
    args foreach (x => println(x+": "+(x.permutations.toList intersect dict).mkString(" ")))
  }

}
