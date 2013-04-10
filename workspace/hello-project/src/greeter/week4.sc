package greeter

object week4 {
	def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)greeter.Cons[T]
	def nth[T](n: Int, xs: List[T]): T =
		if (xs.isEmpty) throw new IndexOutOfBoundsException
		else if (n == 0) xs.head
		else nth(n - 1, xs.tail)          //> nth: [T](n: Int, xs: greeter.List[T])T
		
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : greeter.Cons[Int] = greeter.Cons@683267dd
  nth(2, list)                                    //> res0: Int = 3
  nth(4, list)                                    //> java.lang.IndexOutOfBoundsException
                                                  //| 	at greeter.week4$$anonfun$main$1.nth$1(greeter.week4.scala:6)
                                                  //| 	at greeter.week4$$anonfun$main$1.apply$mcV$sp(greeter.week4.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at greeter.week4$.main(greeter.week4.scala:3)
                                                  //| 	at greeter.week4.main(greeter.week4.scala)
  nth(-1, list)
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class  Nil[T] extends List[T] {
	def isEmpty: Boolean = true
	def head = throw new NoSuchElementException("Nil.head")
	def tail = throw new NoSuchElementException("Nil.tail")
}