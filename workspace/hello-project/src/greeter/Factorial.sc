package greeter

object Factorial {
  def factorial(n: Int): Int = {
  
  	def loop(acc: Int, n: Int): Int =
  		if (n == 0) acc
  		else loop(acc * n, n - 1)
  		
  	loop(1, n)
  }                                               //> factorial: (n: Int)Int
  
  factorial(1)                                    //> res0: Int = 1
  factorial(2)                                    //> res1: Int = 2
  factorial(3)                                    //> res2: Int = 6
  factorial(4)                                    //> res3: Int = 24
  factorial(5)                                    //> res4: Int = 120
}