package greeter

object Sum {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
  	def loop(a: Int, acc: Int): Int = {
  		if (a > b) acc
  		else loop(a + 1, acc + f(a))
  	}
  	loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int
  
  1+2+3+4+5+6                                     //> res0: Int = 21
  sum(x => x)(1, 6)                               //> res1: Int = 21
  
  def product(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 1
  	else f(a) * product(f)(a + 1, b)          //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product(x => x * x)(3, 4)                       //> res2: Int = 144
  
  def fact(n: Int) = product(x => x)(1, n)        //> fact: (n: Int)Int
  
  fact(4)                                         //> res3: Int = 24
  fact(5)                                         //> res4: Int = 120
  fact(6)                                         //> res5: Int = 720
  
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  	if (a > b) zero
  	else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
                                                  
  def product2(f: Int => Int)(a: Int, b: Int): Int =
  	mapReduce(f, (x, y) => x * y, 1)(a, b)    //> product2: (f: Int => Int)(a: Int, b: Int)Int
  
  product2(x => x * x)(3, 4)                      //> res6: Int = 144
}