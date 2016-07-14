import Suggester._
import scala.io.StdIn.readLine
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

  def main(args: Array[String]) {
    val trainingWords = readLine("Enter the training words\n")
    val completer = new iAutoCompleteProvider(trainingWords)
    val stem = readLine("Enter text\n")
    println(completer.getWords(stem))
  }
}
