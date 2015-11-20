import math.abs
object session {
  def fixedPoint(f: Double => Double)(initialGuess: Double): Double = {

    def isGoodEnough(x: Double) =
      abs(x - f(x)) <= x * 1E-2

    def iterate(x: Double) : Double = {
      if (isGoodEnough(x)) x
      else iterate(f(x))
    }

    iterate(initialGuess)
  }
  def averageDamp(f: Double=>Double )(x : Double)= (f(x)+x)/2
  def sqrt(x : Double): Double = fixedPoint(averageDamp(y=>x/y))(1)
  sqrt(2)
}