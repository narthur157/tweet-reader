object intsets {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
  val t1 = new NonEmpty(3, new Empty, new Empty);System.out.println("""t1  : NonEmpty = """ + $show(t1 ));$skip(21); 
  val t2 = t1 incl 4;System.out.println("""t2  : IntSet = """ + $show(t2 ))}
}

abstract class IntSet { // abstract allows us to have functions without shit in them
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  def union(other: IntSet) = ((left union right) union other) incl elem
}
class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x:Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}