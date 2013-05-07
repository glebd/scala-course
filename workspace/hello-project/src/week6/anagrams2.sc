package week6

object anagrams2 {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> lard  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  val r = List(('r', 1))                          //> r  : List[(Char, Int)] = List((r,1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))    //> lad  : List[(Char, Int)] = List((a,1), (d,1), (l,1))
  
  lard.sorted.toMap                               //> res0: scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1, l -> 1,
                                                  //|  r -> 1)
  
  r.toMap.foldLeft(lard.toMap)((a: Map[Char, Int], kv: (Char, Int)) => a.updated(kv._1, kv._2 - a(kv._1))).filter(x => x._2 > 0)
                                                  //> res1: scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1, l -> 1)
                                                  //| 
  

  /*
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val mapped = x.sorted.toMap
  }
  */
  
  //subtract(lard, r)
}