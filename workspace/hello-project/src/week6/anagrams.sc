package week6

object anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  //val dictionary: List[Word] = loadDictionary
  //val word = "Robert"
  //val grouped = word.toLowerCase.groupBy(x => x)
  //val mapped = grouped.mapValues(x => x.length)
  //val occ = mapped.toList.sorted
  //def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(x => x).mapValues(_.length).toList.sorted
  //val dictOcc = dictionary.map(w => (w, wordOccurrences(w)))
  //val dictOccGrouped = dictOcc.groupBy(x => x._2)
  //val dictOccGroupMap = dictOccGrouped.map(x => (x._1, x._2.map(y => y._1)))

  def expand(cf: (Char, Int)): Occurrences = {
    (for {
      n <- 1 to cf._2
    } yield (cf._1, n)).toList
  }                                               //> expand: (cf: (Char, Int))week6.anagrams.Occurrences
  
  expand(('a', 1))                                //> res0: week6.anagrams.Occurrences = List((a,1))
  expand(('a', 2))                                //> res1: week6.anagrams.Occurrences = List((a,1), (a,2))

  def subsets(occurrences: Occurrences): Occurrences = {
    occurrences match {
      case Nil => Nil
      case List((c, n)) => List((c, n))
      case list =>
        val len = occurrences.length
        (for {
          l <- 1 to len
          s <- subsets(list.drop(l))
        } yield s).toList
    }
  }                                               //> subsets: (occurrences: week6.anagrams.Occurrences)week6.anagrams.Occurrence
                                                  //| s

  val abba = List(('a', 2), ('b', 2))             //> abba  : List[(Char, Int)] = List((a,2), (b,2))
  
  subsets(abba)                                   //> res2: week6.anagrams.Occurrences = List((b,2))

  /*
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
      case Nil => List(List())
      case List((c, n)) => List(subsets((c, n))
      case head :: tail => subsets(head) ::: combinations(tail)
    }
  }
  */

  val abbacomb = List(
    List(),
    List(('a', 1)),
    List(('a', 2)),
    List(('b', 1)),
    List(('a', 1), ('b', 1)),
    List(('a', 2), ('b', 1)),
    List(('b', 2)),
    List(('a', 1), ('b', 2)),
    List(('a', 2), ('b', 2)))                     //> abbacomb  : List[List[(Char, Int)]] = List(List(), List((a,1)), List((a,2))
                                                  //| , List((b,1)), List((a,1), (b,1)), List((a,2), (b,1)), List((b,2)), List((a
                                                  //| ,1), (b,2)), List((a,2), (b,2)))
  //combinations(abba).toSet
  abbacomb.toSet                                  //> res3: scala.collection.immutable.Set[List[(Char, Int)]] = Set(List((a,1)), 
                                                  //| List((b,1)), List((b,2)), List((a,1), (b,1)), List(), List((a,2), (b,1)), L
                                                  //| ist((a,2)), List((a,1), (b,2)), List((a,2), (b,2)))
}