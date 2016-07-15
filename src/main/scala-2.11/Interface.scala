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

  //TODO: modify to accept stdin or file names of training sets. Keep the text input alive.
  /**
    * Provides user interface via the command line.
    *
    *
    * @param args none
    */
  def main(args: Array[String]) {
    //val trainingWords = readLine("Enter the training words\n")
    val trainingWords = "cow cat dog duck"
    val completer = new iAutoCompleteProvider(trainingWords)
    //val prefix = readLine("Enter text\n")
    val prefix = "c"
    println(completer.getWords(prefix))
  }
}
