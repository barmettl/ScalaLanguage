object session {
  class Rational(n: Int, d : Int){
    require(d!=0,"Deonominator must be non-zero")

    private def gcd(a: Int, b: Int):Int = if(b==0) a else gcd(b,a % b)
    private val divisor = gcd(n,d)
    val numerator = n/divisor
    val denominator = d/divisor

    def +(that: Rational) =
      new Rational( numerator*that.denominator + denominator*that.numerator,
        denominator*that.denominator )
    def unary_- = new Rational(-numerator,denominator)
    override def toString() = numerator + "/" + denominator
  }
  var r = new Rational(1,2)
  r.denominator
  r.numerator
  r + new Rational(2,2)
  r + -r
  new Rational(1,1)
}