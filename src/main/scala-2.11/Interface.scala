import Suggester._
import scala.io.Source


/**
  * Created by joshuaarnold on 7/14/16.
  */
object Interface {

  /**
    * An autocompleter.
    *
    * @param words the single String of training words, separated by non-alphabetic characters
    */
  class iAutoCompleteProvider(words: String) {
    private val trainedTree: Tree = createTree(words)
    def getWords(root: String) = trainedTree getSuggestions root
  }

  //TODO: modify to accept file names of training and test words. Keep the text input alive.
  /**
    * Provides user interface via the command line.
    *
    *
    * @param args none
    */
  def main(args: Array[String]) {
    val trainingFileName = "trainingWords.txt"
    val testFileName = "testWords.txt"

    lazy val trainingWords =
      Source.fromURL(getClass.getResource(trainingFileName)).getLines.toList.map(_ ++ " ").mkString

    val completer = new iAutoCompleteProvider(trainingWords)

    lazy val testWords =
      Source.fromURL(getClass.getResource(testFileName)).getLines.toList.map(_ ++ " ").mkString

    for (prefix <- testWords.split(" ")) {
      print(prefix + " -> ")
      println(completer.getWords(prefix))
    }
  }
}
