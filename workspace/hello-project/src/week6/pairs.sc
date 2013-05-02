package week6

object pairs {
	val n = 7                                 //> n  : Int = 7
	
	(1 until n) map (i =>
		(1 until i) map (j => (i, j)))    //> res0: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Indexe
                                                  //| dSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), Vec
                                                  //| tor((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1), (
                                                  //| 6,2), (6,3), (6,4), (6,5)))
		
	((1 until n) map (i =>
		(1 until i) map (j => (i, j)))).flatten
                                                  //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5))
		
	(1 until n) flatMap (i =>
	  (1 until i) map (j => (i, j))) filter (pair =>
	    	isPrime(pair._1 + pair._2))       //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
                                                  
  for {
  	i <- 1 until n
  	j <- 1 until i
  	if isPrime(i + j)
  } yield (i, j)                                  //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
}