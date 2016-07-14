/** Left Child Right Sibling binary tree
  *
  */


abstract class Tree {
  def parse: List[String]
  def merge(chars: List[Char]): Tree
  def contains(chars: List[Char]): Boolean
}

object Empty extends Tree {
  override def toString = "."
  def parse: List[String] = List()
  def merge(chars: List[Char]): Tree = chars match {
    case List() => this
    case x :: Nil => new NonEmpty(x, 1, Empty, Empty)
    case x :: xs => new NonEmpty(x, 0, Empty merge xs, Empty)
  }
  def contains(chars: List[Char]): Boolean = false
}

case class NonEmpty(char: Char, weight: Int, left: Tree, right: Tree) extends Tree {
  override def toString = "{" + left + char + "(" + weight + ")" + right + "}"

  def merge(chars: List[Char]): Tree = chars match {
    case List() => this
    case x :: Nil =>
      if (x == char)
        new NonEmpty(char, weight, left, right)
      else
        new NonEmpty(char, weight, left, right merge chars )
    case x :: xs =>
      if (x == char)
        new NonEmpty(char, weight, left merge xs, right)
      else
        new NonEmpty(char, weight, left, right merge chars)
  }

  def parse: List[String] = {
    def parseAcc(subtree: Tree, root: List[Char]): List[String] = subtree match {
      case Empty => List(root.mkString)
      case NonEmpty(c, w, l, Empty) => parseAcc(l,root ::: List(c))
      case NonEmpty(c, w, l, r) => parseAcc(l,root ::: List(c)) ++ parseAcc(r, root)
    }
    parseAcc(this,List())
  }

  def contains(chars: List[Char]): Boolean = {
    def loop(subtree: Tree, xs: List[Char], acc: Boolean): Boolean = subtree match {
      case Empty => acc
      case NonEmpty(c, _, Empty, Empty) => xs match {
        case List() => true
        case x :: Nil => c == x
        case x :: xs1 => false
      }
      case NonEmpty(c, _, l, Empty) => xs match {
        case List() => acc
        case x :: Nil => (c == x) && acc
        case x :: xs1 => if (c == x) loop(l, xs1, acc) else false
      }
      case NonEmpty(c, _, l, r) => xs match {
        case List() => acc
        case x :: Nil => acc && (c == x)
        case x :: xs1 => if (c == x) loop(l, xs1, acc) else loop(r, xs, acc)
      }
    }
    loop(this, chars, true)
  }

}

val sentence = "The third thing that I need to tell you is that this thing does not think thoroughly."

def createTree(sentence: String): Tree = {
  val acc: Tree = Empty
  ((sentence.toLowerCase().replaceAll("[^a-z]"," ") split " " ) foldLeft acc)(_ merge _.toList)
}

val farm = createTree("Cat cow 1234;_cattle dog dart.")
farm parse

def expand(word: String): List[List[Char]] = (for (i <- 1 to word.length) yield word take(i) toList).toList

val q = expand("darts")
q map(farm contains _)

val q2 = expand("dogs")
q2 map(farm contains _)
