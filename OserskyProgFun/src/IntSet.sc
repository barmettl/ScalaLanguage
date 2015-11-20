object session {
  abstract class IntSet {
    def contains(x: Int):Boolean
    def insert(value: Int): IntSet
    def union(s: IntSet): IntSet
  }
  object EmptyNode  extends IntSet {
    def contains(x: Int) = false
    def insert(x: Int) = new IntNode(x,EmptyNode,EmptyNode)
    override def toString = ""
    def union(s: IntSet): IntSet = s
  }

  class IntNode(x: Int, val left: IntSet,val right: IntSet)
    extends IntSet {
    def this(x: Int) = this(x,EmptyNode,EmptyNode)
    var value = x
    override def contains(x: Int): Boolean = {
      if(x<value) left.contains(x)
      else if(x>value) right.contains(x)
      else true
    }
    override def insert(x: Int): IntSet = {
      if( x < value ) new IntNode(value,left.insert(x),right)
      else new IntNode(value,left,right.insert(x))
    }
    override def toString() = "{"+ value +
      left.toString() + right.toString()+"}"

    def union(s: IntSet): IntSet = {
      ((left union right ) union s).insert(value)
    }
  }
  var set =new IntNode(7)
  set.contains(7)
  set.contains(8)
  var s1=set.insert(8).insert(6).insert(7).insert(8)
  var s2 = set.insert(2)
  s1 union s2
}