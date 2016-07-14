/** Left Child Right Sibling binary tree
  *
  */

class Elem(val c: Char, val w: Int) {
  def < (that: Elem): Boolean = this.c < that.c
  def > (that: Elem): Boolean = this.c > that.c
  override def toString = c + "(" + this.w + ")"
  //override def toString = c.toString
}

abstract class Tree {
  def parse: List[String]
  def merge(chars: List[Char]): Tree
  def dum: Tree
}

object Empty extends Tree {
  override def toString = "."
  def parse: List[String] = List()
  def merge(chars: List[Char]): Tree = chars match {
    case List() => this
    case x :: Nil => new NonEmpty(new Elem(x, 1), Empty, Empty)
    case x :: xs => new NonEmpty(new Elem(x, 1), Empty merge(xs), Empty)
  }
  def dum: Tree = this
}

case class NonEmpty(elem: Elem, left: Tree, right: Tree) extends Tree {
  override def toString = "{" + left + elem + right + "}"

  def merge(chars: List[Char]): Tree = chars match {
    case List() => this
    case x :: Nil =>
      if (x == elem.c)
        new NonEmpty(new Elem(elem.c, elem.w+1), left, right)
      else
        new NonEmpty(elem, left, right merge chars )
    case x :: xs =>
      if (x == elem.c)
        new NonEmpty(elem, left merge xs, right)
      else
        new NonEmpty(elem, left, right merge chars)
  }

  /*
  def parse: List[String] = {
    def parseAcc(subtree: Tree, root: List[Char]): List[String] = subtree match {
      case Empty => Nil
      case NonEmpty(e, Empty, Empty) => List((root ::: List(e.c)) mkString)
      case NonEmpty(e, l, Empty) => parseAcc(l,root ::: List(e.c))
      case NonEmpty(e, l, r) => parseAcc(l,root ::: List(e.c)) ++ parseAcc(r, root)
    }
    parseAcc(this,List())
  }
  */

  def parse: List[String] = {
    def parseAcc(subtree: Tree, root: List[Char]): List[String] = subtree match {
      case Empty => List(root.mkString)
      case NonEmpty(e, l, Empty) => parseAcc(l,root ::: List(e.c))
      case NonEmpty(e, l, r) => parseAcc(l,root ::: List(e.c)) ++ parseAcc(r, root)
    }
    parseAcc(this,List())
  }
  def dum: Tree = new NonEmpty(elem, right, Empty)

}


def singleton(c: Char, w: Int) = new NonEmpty(new Elem(c,w), Empty, Empty)


val tree1 = Empty merge("cat" toList)
tree1.parse

val tree2 = tree1 merge("dog" toList)
tree2.parse

val tree3 = tree2 merge("cow" toList)
tree3.parse

List('a','b','c').mkString

val sentence = "The third thing that I need to tell you is that this thing does not think thoroughly."

//(sentence.toLowerCase() split " " foldLeft Empty)((a,b) => a.merge)
val acc: Tree = Empty
val test1 = ((sentence.toLowerCase() split " " ) foldLeft acc)(_ merge _.toList)

test1.parse

def createTree(sentence: String): Tree = {
  val acc: Tree = Empty
  ((sentence.toLowerCase().replaceAll("[^a-z]"," ") split " " ) foldLeft acc)(_ merge _.toList)
}

createTree(sentence) parse

