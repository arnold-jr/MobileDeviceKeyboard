

/**
  * Created by joshuaarnold on 7/13/16.
  *
  * Contains the essential functionality for building a Left Child Right Sibling binary tree
  */
object Suggester {

  /**
    * The fundamental class of a possible word completion and its confidence value.
    *
    * @param word the suggested word
    * @param confidence the number of times that word appeared in the training set
    */
  case class Hint(word: String, confidence: Int) {
    override def toString = word + " (" + confidence + ")"
  }

  /**
    * Abstract base class type representing a LCRS binary tree
    */
  abstract class Tree {
    /**
      * Merges the List of Char representing a single word into this tree.
      *
      * Merging starts at the root, and the last character in "chars" increments by 1 the
      * weight of the terminal NonEmpty Tree.
      *
      * @param chars the list of characters to be merged
      * @return the resultant treee
      */
    def merge(chars: List[Char]): Tree

    /**
      * Traverses a tree returning valid words (or suffixes) within a tree.
      *
      * Each NonEmpty element within a tree is defined by a character and a weight. The weight
      * denotes the number of times a string has ended with that character.
      *
      * @return the List of Strings representing valid sub-words
      */
    def parse: List[Hint]

    /**
      * Tests whether this Tree contains a String, starting from the root. Implemented and tested,
      * but not utilized.
      *
      * @param word the String to be queried
      * @return true if the (sub) word exists in the string
      */
    def contains(word: String): Boolean

    /**
      * Gets all valid suggestions based on the training vocabulary.
      *
      * @param word the String to be queried
      * @return the resultant Tree, a subset of this Tree
      */
    def getSuggestions(word: String): List[Hint]

  }

  /**
    * The invariant Empty Tree
    */
  object Empty extends Tree {
    override def toString = "."


    /**
      * Merges new word (List[Char]) into an Empty Tree.
      *
      * Creates a child (left neighbor) for each character and assigns a confidence (weight) of 1
      * to the last character, signifying the end of valid word. It is necessary to mark the
      * end of valid words because valid words do not necessarily terminate at leaves. For instance,
      * the word "cat" maps onto the word "cattle" and terminates at a non-leaf element (i.e. "t"
      * has a NonEmpty left neighbor).
      *
      * @param chars the list of characters to be merged
      * @return the resultant treee
      */
    def merge(chars: List[Char]): Tree = chars match {
      case List() => this
      case x :: Nil => new NonEmpty(x, 1, Empty, Empty)
      case x :: xs => new NonEmpty(x, 0, Empty merge xs, Empty)
    }

    def parse: List[Hint] = List()

    def contains(word: String): Boolean = false

    def getSuggestions(word: String): List[Hint] = List()
  }

  /**
    * A Tree containing data, children, and siblings
    *
    * @constructor creates a new Tree with character, weight, left child, and right sibling
    * @param char   the character at this point at the tree
    * @param weight the number of times a valid word terminates at this node
    * @param left   Tree of the left children of this Tree
    * @param right  Tree of the right neigbhors of this Tree
    */
  case class NonEmpty(char: Char, weight: Int, left: Tree, right: Tree) extends Tree {
    override def toString = "{" + left + char + "(" + weight + ")" + right + "}"

    /**
      * Merges new word (List[Char]) into the existing NonEmpty Tree.
      *
      * Recurses through both the word and this Tree, checking if "char" of This tree matches
      * the head of the "chars" list. If so, both are 'popped' and the rest of the tree is traversed.
      * If not, siblings (right neighbors) are searched for matches.
      *
      * @param chars the list of characters to be merged
      * @return the resultant treee
      */
    def merge(chars: List[Char]): Tree = chars match {
      case List() => this
      case x :: Nil =>
        if (x == char)
          new NonEmpty(char, weight + 1, left, right)
        else
          new NonEmpty(char, weight, left, right merge chars)
      case x :: xs =>
        if (x == char)
          new NonEmpty(char, weight, left merge xs, right)
        else
          new NonEmpty(char, weight, left, right merge chars)
    }

    /**
      * Traverses a tree returning valid words (or suffixes) within a tree.
      *
      * When the recursion encounters a non-zero weight (confidence), that corresponding
      * hint is constructed and added to the output List.
      *
      * @return the List of Hints representing valid sub-words (i.e. without root)
      */
    def parse: List[Hint] = {
      def parseAcc(subtree: Tree, root: List[Char]): List[Hint] = subtree match {
        case Empty => Nil
        case NonEmpty(c, 0, l, r) => parseAcc(l, root ::: List(c)) ++ parseAcc(r, root)
        case NonEmpty(c, w, l, r) => List(new Hint((root ::: List(c)).mkString,w)) ++
          parseAcc(l, root ::: List(c)) ++ parseAcc(r, root)
      }
      parseAcc(this, List())
    }

    /**
      *
      *
      * Here, the Tree is traversed concurrently with "word". If the root of the Tree
      * matches the head of the string, both are 'popped' and the search continues. If the root
      * of the Tree does not match the head, the word is compared to the right 'sibling' Tree.
      *
      * @param word the String to be queried
      * @return true if the (sub) word exists in the string
      */
    def contains(word: String): Boolean = {
      def loop(subtree: Tree, xs: List[Char]): Boolean = subtree match {
        case Empty => false
        case NonEmpty(c, _, Empty, Empty) => xs match {
          case List() => true
          case x :: Nil => c == x
          case x :: xs1 => false
        }
        case NonEmpty(c, _, l, Empty) => xs match {
          case List() => true
          case x :: Nil => c == x
          case x :: xs1 => if (c == x) loop(l, xs1) else false
        }
        case NonEmpty(c, _, l, r) => xs match {
          case List() => true
          case x :: Nil => if (c == x) true else loop(r, xs)
          case x :: xs1 => if (c == x) loop(l, xs1) else loop(r, xs)
        }
      }
      loop(this, word.toList)
    }


    /**
      * Gets all valid suggestions based on the training vocabulary.
      *
      * The query string and the Tree are both traversed recursively; if the entire query String
      * is found within the tree, then all valid suffixes are found by calling parse on the children
      * of the subtree. In the final steps, the list of suffixes (as Hints) are prepended with the
      * query String, then sorted descending by confidence then by lexicographical ascension.
      *
      * @param word the String to be queried
      * @return the resultant Tree, a subset of this Tree
      */
    def getSuggestions(word: String): List[Hint] = {
      def loop(subtree: Tree, xs: List[Char]): List[Hint] = subtree match {
        case NonEmpty(c, _, Empty, Empty) => xs match {
          case List() => subtree parse
          case x :: Nil => List()
          case x :: xs1 => List()
        }
        case NonEmpty(c, _, l, Empty) => xs match {
          case List() => subtree parse
          case x :: Nil => if (c == x) l parse else List()
          case x :: xs1 => if (c == x) loop(l, xs1) else List()
        }
        case NonEmpty(c, _, l, r) => xs match {
          case List() => subtree parse
          case x :: Nil => if (c == x) l parse else loop(r, xs)
          case x :: xs1 => if (c == x) loop(l, xs1) else loop(r, xs)
        }
      }
      //TODO: Look into sorting on the fly with heap sort
      loop(this, word.toList) map {case Hint(w, c) => new Hint(word + w, c)} sortBy(h => (- h.confidence, h.word))
    }
  }

  /**
    * Creates a LCRS Tree from all words in a sentence.
    *
    * Words are assumed to be alphabet characters separated by any non-alphabet character.
    * foldLeft is used as the combiner which starts from the Empty Tree and merges each
    * word in the sentence.
    *
    * @param sentence the string with which to create a tree
    * @return the resultant Tree
    */
  def createTree(sentence: String): Tree = {
    val acc: Tree = Empty
    ((sentence.toLowerCase().replaceAll("[^a-z]", " ") split " ") foldLeft acc) (_ merge _.toList)
  }

}
