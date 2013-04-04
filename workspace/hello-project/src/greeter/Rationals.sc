package greeter

object Rationals {
	val x0 = new Rational(1, 2)               //> x0  : greeter.Rational = 1/2
	x0.numer                                  //> res0: Int = 1
	x0.denom                                  //> res1: Int = 2
	
	val y0 = new Rational(2, 3)               //> y0  : greeter.Rational = 2/3
	val z0 = x0.add(y0)                       //> z0  : greeter.Rational = 7/6
	x0.neg                                    //> res2: greeter.Rational = -1/2
	
	val x = new Rational(1, 3)                //> x  : greeter.Rational = 1/3
	val y = new Rational(5, 7)                //> y  : greeter.Rational = 5/7
	val z = new Rational(3, 2)                //> z  : greeter.Rational = 3/2
	
	x.sub(y).sub(z)                           //> res3: greeter.Rational = -79/42
}

class Rational(x: Int, y: Int) {
	def numer = x
	def denom = y
	
	def add(that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
			
	def neg: Rational = new Rational(-numer, denom)
	
	def sub(that: Rational) =	add(that.neg)
			
	override def toString = numer + "/" + denom
}