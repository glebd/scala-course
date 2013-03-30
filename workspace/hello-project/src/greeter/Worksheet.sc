package greeter

object Worksheet {
  def sqrt(x: Double) = {

    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
      
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 1.0e-3

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res0: Double = 1.4142156862745097
  sqrt(4)                                         //> res1: Double = 2.000609756097561
  sqrt(1.0e-6)                                    //> res2: Double = 0.0010000001533016628
  sqrt(1.0e60)                                    //> res3: Double = 1.0000788456669446E30
}