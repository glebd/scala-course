package greeter

object Rationals {
	val x0 = new Rational(1, 2)               //> x0  : greeter.Rational = 1/2
	x0.numer                                  //> res0: Int = 1
	x0.denom                                  //> res1: Int = 2
	
	val y0 = new Rational(2, 3)               //> y0  : greeter.Rational = 2/3
	val z0 = x0 + y0                          //> z0  : greeter.Rational = 7/6
	-x0                                       //> res2: greeter.Rational = 1/-2
	
	val x = new Rational(1, 3)                //> x  : greeter.Rational = 1/3
	val y = new Rational(5, 7)                //> y  : greeter.Rational = 5/7
	val z = new Rational(3, 2)                //> z  : greeter.Rational = 3/2
	
	x - y - z                                 //> res3: greeter.Rational = -79/42
	y + y                                     //> res4: greeter.Rational = 10/7
	x < y                                     //> res5: Boolean = true
	x max y                                   //> res6: greeter.Rational = 5/7
	//val strange = new Rational(1, 0)
	new Rational(2)                           //> res7: greeter.Rational = 2/1
}

class Rational(x: Int, y: Int) {
	require(y != 0, "Denominator must be non-zero")
	
	def this(x: Int) = this(x, 1)

	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	def numer = x
	def denom = y
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if (this < that) that else this
	
	def + (that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
			
	def unary_- : Rational = new Rational(-numer, denom)
	
	def - (that: Rational) =	this + -that
			
	override def toString = {
		val g = gcd(numer, denom)
	  numer / g + "/" + denom / g
	}
}