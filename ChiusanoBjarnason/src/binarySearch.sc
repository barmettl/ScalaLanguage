import scala.annotation.tailrec

object session {

  def binarySearch[T](a: Array[T],value: T, lesserThan: (T,T)=>Boolean) ={

    @tailrec
    def go(first: Int,last: Int):Int={
      if(first>last) -1
      else {
        val c = (last + first) / 2
        if(a(c)==value) c
        else if ( lesserThan( a(c),value) ) go(first, c - 1)
        else go(c,last)
      }
    }
    go(0,a.length-1)
  }

  def isSorted[T](a: Array[T],gr:(T,T)=>Boolean) = {
    def go(i:Int):Boolean ={
      if(i>=a.length-1) true
      else if(gr(a(i),a(i+1))) false
      else go(i+1)
    }
    go(0)
  }

  val a = Array(1,2,3,4)
  
  binarySearch(a,2,(x:Int,y:Int)=> x<y)
  isSorted(a,(x:Int,y:Int)=>x>y);
}