import scala.io.Source

object Anagram {  

  val DICT_FILE = "/usr/share/dict/words"

  def main(args: Array[String]) {    
    val dict = Source.fromFile(DICT_FILE).getLines().toList
    args foreach (x => println(x+": "+(x.permutations.toList intersect dict).mkString(" ")))
//    args foreach (x => println(x+": "+(x.permutations.toList intersect Source.fromFile("/usr/share/dict/words").getLines().toList).mkString(" ")))
  }

}
