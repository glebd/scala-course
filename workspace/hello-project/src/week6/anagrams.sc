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
  }
  
  expand(('a', 1))
  expand(('a', 2))

  def subsets(occurrences: Occurrences): Occurrences = {
    occurrences match {
      case Nil => Nil
      case List((c, n)) => List((c, n))
      case head :: tail =>
        
    }
  }

  val abba = List(('a', 2), ('b', 2))
  
  subsets(abba)

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
    List(('a', 2), ('b', 2)))
  //combinations(abba).toSet
  abbacomb.toSet
}