import week8._

object randtest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }                                               //> integers  : week8.Generator[Int] = randtest$$anonfun$main$1$$anon$1@38dda25b
                                                  //| 
  def single[T](x: T) = new Generator[T] {
    def generate = x
  }                                               //> single: [T](x: T)week8.Generator[T]
  
  val booleans = integers.map(_ >= 0)             //> booleans  : week8.Generator[Boolean] = week8.Generator$$anon$1@77fddc31
  
  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield(x, y)                                   //> pairs: [T, U](t: week8.Generator[T], u: week8.Generator[U])week8.Generator[(
                                                  //| T, U)]
  
  def emptyLists = single(Nil)                    //> emptyLists: => week8.Generator[scala.collection.immutable.Nil.type]
  
  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail                            //> nonEmptyLists: => week8.Generator[List[Int]]
  
  def lists: Generator[List[Int]] = for {
    cutoff <- booleans
    list <- if (cutoff) emptyLists else nonEmptyLists
  } yield list                                    //> lists: => week8.Generator[List[Int]]
  
  def test[T](r: Generator[T], noTimes: Int = 100)(test: T => Boolean) {
    for (_ <- 0 until noTimes) {
      val value = r.generate
      assert(test(value), "Test failed for: " + value)
    }
    println("Test passed " + noTimes + " times")
  }                                               //> test: [T](r: week8.Generator[T], noTimes: Int)(test: T => Boolean)Unit
  
  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }                                               //> java.lang.AssertionError: assertion failed: Test failed for: (List(69642357
                                                  //| 8, 1745363770, 1441493375),List())
                                                  //| 	at scala.Predef$.assert(Predef.scala:179)
                                                  //| 	at randtest$$anonfun$main$1$$anonfun$test$1$1.apply$mcVI$sp(randtest.sca
                                                  //| la:35)
                                                  //| 	at randtest$$anonfun$main$1$$anonfun$test$1$1.apply(randtest.scala:33)
                                                  //| 	at randtest$$anonfun$main$1$$anonfun$test$1$1.apply(randtest.scala:33)
                                                  //| 	at scala.collection.immutable.Range.foreach(Range.scala:141)
                                                  //| 	at randtest$$anonfun$main$1.test$1(randtest.scala:33)
                                                  //| 	at randtest$$anonfun$main$1.apply$mcV$sp(randtest.scala:40)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at randtest$.main(randtest.scala:3)
}