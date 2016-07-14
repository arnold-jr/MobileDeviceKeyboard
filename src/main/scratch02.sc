/** Left Child Right Sibling binary tree
  *
  */

class Elem(val c: Char, val w: Int) {
  def < (that: Elem): Boolean = this.c < that.c
  def > (that: Elem): Boolean = this.c > that.c
  override def toString = c + "(" + this.w + ")"
}

abstract class Tree {
  def addChild(x: Elem): Tree
  def addSibling(x: Elem): Tree
  def parseTree: List[Char]
  def merge(chars: List[Char]): Tree
}

object Empty extends Tree {
  def addChild(x: Elem): Tree = new NonEmpty(x, Empty, Empty)
  def addSibling(x: Elem): Tree = new NonEmpty(x, Empty, Empty)
  def parseTree = Nil
  def merge(chars: List[Char]): Tree = this
  override def toString = "."
}

class NonEmpty(val elem: Elem, left: Tree, right: Tree) extends Tree {
  def addChild(x: Elem): Tree =
    new NonEmpty(elem, left addChild(x), right)
  def addSibling(x: Elem): Tree =
    new NonEmpty(elem, left, right addSibling(x))
  override def toString = "{" + left + elem + right + "}"

  def merge(chars: List[Char]): Tree = chars match {
      case List() => this
      case x :: xs =>
        if (x == elem.c) this.merge(xs)
        else if (x > elem.c) this.merge(xs)
        else this.merge(xs)
  }

  def parseTree: List[Char] = {
    elem.c :: left.parseTree
  }
}

val t1 = new NonEmpty(new Elem('c',0), Empty, Empty)

t1 addChild new Elem('a',0) addChild new Elem('t',1)

def buildTree(word: String, weight: Int): Tree = {
  def loop(chars: List[Char], acc: Tree): Tree = chars match {
    case List() => acc
    case x :: Nil => acc addChild(new Elem(x, weight))
    case x :: xs => loop(xs, acc addChild(new Elem(x, 0)))
  }
  loop(word.toList, Empty)
}

val tree1 = buildTree("cat",1)
val tree2 = buildTree("car",1)

tree1.parseTree
tree2.parseTree

val deb = None: Option[Char]

val lesa = List('a','b')

tree1.addSibling(new Elem('d',2))
tree1.merge("dog".toList)