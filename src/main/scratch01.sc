type Elem = Char

abstract class Tree {
  def incl(x: Elem): Tree
  def contains(x: Elem): Boolean
  def union(other: Tree): Tree
}

object Empty extends Tree {
  def contains(x: Elem): Boolean = false
  def incl(x: Elem): Tree = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: Tree): Tree = other
}

class NonEmpty(elem: Elem, left: Tree, right: Tree) extends Tree {
  def contains(x: Elem): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Elem): Tree =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  def union(other: Tree): Tree =
    ((left union right) union other) incl elem
}

val t1 = new NonEmpty('c', Empty, Empty)
val t2 = t1 incl 'd'
val t3 = new NonEmpty('e', Empty, Empty)

t3 union t2
