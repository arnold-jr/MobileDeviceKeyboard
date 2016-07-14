
val foo = "ax adze" split(" ") toList


/** Counts the occurrences of words in a sentence.
  *
  *
  * @param sentence the sentence to be parsed, separated with whitespace
  * @return map of words and their frequencies
  */
def counter(sentence: String): Map[String, Int] = {
  def addEntry(entries: Map[String, Int], entry: String): Map[String, Int] = {
    entries + (entry -> (1 + entries.getOrElse(entry, 0)))
  }
  //TODO: make splitting more robust to punctuation
  lazy val words = sentence.toLowerCase split(" ") toList
  val acc: Map[String,Int] = Map()
  (words foldLeft acc)(addEntry)
}

val sentence = "George George George of the jungle"
val occurrences = counter(sentence)

def perms(word: String): Map[String,String] =
  (for (i <- 1 until word.length) yield ((word take(i)) -> word)) toMap

perms("adze")

occurrences.toList

sentence split(" ") flatMap(perms(_))