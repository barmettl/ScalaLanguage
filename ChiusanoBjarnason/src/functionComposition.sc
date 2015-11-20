import  scala.math.pow
object session {
  def partial[A,B,C](a:A, f: (A,B)=>C): B => C ={
    (b:B)=>f(a,b)
  }
  val pow2=partial(2.0,pow)
  val p = pow2(3.0)
  def currying[A,B,C](f:(A,B)=>C)={
    (a: A)=>((b:B)=>f(a,b))
  }
  var cPow= currying(pow)
  cPow(2)(3)

  def uncurry[A,B,C](f: (A=>(B=>C))) = {
    (a: A, b: B)=> f(a)(b)
  }
  var uPow =uncurry(cPow)
  uPow(2,3)
  def compose[A,B,C](f: A=>B,g: B=>C)={
    (a: A)=>g(f(a))
  }
  val plusOneTimes2 = compose(
    partial(1,(a: Int,b: Int)=>a+b),
    partial(2,(a: Int, b: Int)=>a*b))
 plusOneTimes2(2)
}