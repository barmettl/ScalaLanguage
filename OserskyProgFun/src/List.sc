object session {

  trait List[+T] {
    def isEmpty : Boolean
    def value: T
    def tail: List[T]
    def get(idx:Int):T = {
      if(isEmpty) throw new IndexOutOfBoundsException
      else if(idx==0) value else tail.get(idx)
    }
    def prepend[U >: T](u:U) = new Cons(u,this)
  }

  object Nil extends List[Nothing] {
    def isEmpty = true
    def value: Nothing = throw new NoSuchElementException("list.value")
    def tail: List[Nothing] = throw new NoSuchElementException("list.tail")
    override def toString = ""
  }

  class Cons[T](val value:T,val tail: List[T]) extends List[T] {
    def isEmpty = false
    override def toString =
        value.toString + (if(tail.isEmpty) "" else " , "+ tail.toString)
  }

  val nil = Nil
  def singleElementList[T](x:T) = new Cons[T](x,Nil)
  singleElementList(2)
  val l = new Cons[Int](1,new Cons[Int](2,Nil))
  l.prepend("Bla bla bla")

}