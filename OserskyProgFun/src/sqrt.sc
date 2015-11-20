import scala.annotation.tailrec
object session {
  def sqrt(value: Double) ={

    val relativeToleranceSquare = 1E-4*1E-4
    val maxNumSteps = 100
    @tailrec
    def sqrtIterate(guess: Double, step: Int): Double =
      if (converged (guess) || step >= maxNumSteps) guess
      else sqrtIterate (improve (guess),step+1 )

    def converged(guess: Double): Boolean =
      ((value-guess*guess)*(value-guess*guess)) <
        guess*guess*guess*guess*relativeToleranceSquare

    def improve(guess: Double): Double =
      0.5*(guess + (value / guess))

    sqrtIterate(1.0,0)
  }
  sqrt(2)
}
