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

  def expandMany(occurrences: Occurrences): Occurrences = {
    occurrences match {
      case Nil => Nil
      case h :: t =>
        expand(h) ::: expandMany(t)
    }
  }                                               //> expandMany: (occurrences: week6.anagrams.Occurrences)week6.anagrams.Occurren
                                                  //| ces
  expandMany(List(('a', 1)))                      //> res2: week6.anagrams.Occurrences = List((a,1))
  expandMany(List(('a', 2)))                      //> res3: week6.anagrams.Occurrences = List((a,1), (a,2))
  expandMany(List(('a', 3)))                      //> res4: week6.anagrams.Occurrences = List((a,1), (a,2), (a,3))

  def interleave(occ: Occurrences, o: (Char, Int)): List[Occurrences] = {
    occ match {
      case Nil => List(List(o))
      case h :: t => (o :: h :: t) :: interleave(t, o).map(h :: _)
    }
  }                                               //> interleave: (occ: week6.anagrams.Occurrences, o: (Char, Int))List[week6.ana
                                                  //| grams.Occurrences]

  def permutations(occ: Occurrences): List[Occurrences] = {
    occ match {
      case Nil => List(List())
      case h :: t =>
        for {
          p0 <- permutations(t)
          p1 <- interleave(p0, h)
        } yield p1
    }
  }                                               //> permutations: (occ: week6.anagrams.Occurrences)List[week6.anagrams.Occurren
                                                  //| ces]

  def subsets(occurrences: Occurrences): List[Occurrences] = {
    (for {
      n <- 0 to occurrences.length
      combo <- occurrences.combinations(n)
    } yield combo).toList
  }                                               //> subsets: (occurrences: week6.anagrams.Occurrences)List[week6.anagrams.Occur
                                                  //| rences]

  val abba = List(('a', 2), ('b', 2))             //> abba  : List[(Char, Int)] = List((a,2), (b,2))

  val s = subsets(abba)                           //> s  : List[week6.anagrams.Occurrences] = List(List(), List((a,2)), List((b,2
                                                  //| )), List((a,2), (b,2)))

  expandMany(abba)                                //> res5: week6.anagrams.Occurrences = List((a,1), (a,2), (b,1), (b,2))
  val expanded = for {
    s <- subsets(abba)
  } yield expandMany(s)                           //> expanded  : List[week6.anagrams.Occurrences] = List(List(), List((a,1), (a,
                                                  //| 2)), List((b,1), (b,2)), List((a,1), (a,2), (b,1), (b,2)))

  permutations(abba)                              //> res6: List[week6.anagrams.Occurrences] = List(List((a,2), (b,2)), List((b,2
                                                  //| ), (a,2)))

  expanded.combinations(2) mkString ("\n")        //> res7: String = List(List(), List((a,1), (a,2)))
                                                  //| List(List(), List((b,1), (b,2)))
                                                  //| List(List(), List((a,1), (a,2), (b,1), (b,2)))
                                                  //| List(List((a,1), (a,2)), List((b,1), (b,2)))
                                                  //| List(List((a,1), (a,2)), List((a,1), (a,2), (b,1), (b,2)))
                                                  //| List(List((b,1), (b,2)), List((a,1), (a,2), (b,1), (b,2)))

  def permute2(occ: Occurrences): List[Occurrences] =
    occ match {
      case Nil => Nil
      case h :: t => permutations(expand(h)) ::: permute2(t)
    }                                             //> permute2: (occ: week6.anagrams.Occurrences)List[week6.anagrams.Occurrences]
                                                  //| 

  permute2(abba)                                  //> res8: List[week6.anagrams.Occurrences] = List(List((a,1), (a,2)), List((a,2
                                                  //| ), (a,1)), List((b,1), (b,2)), List((b,2), (b,1)))

  /*
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val occ = subsets(occurrences)
    occ match {
      case Nil => List(List())
      case h :: t =>
        val exp = for {
          o <- h
        } yield expand(o)
    }
  }
  */

  def allPossibleOccurrences(occ: Occurrences): Occurrences = {
    expandMany(occ)
  }                                               //> allPossibleOccurrences: (occ: week6.anagrams.Occurrences)week6.anagrams.Occ
                                                  //| urrences

  allPossibleOccurrences(List(('a', 2)))          //> res9: week6.anagrams.Occurrences = List((a,1), (a,2))

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations1(occurrences: Occurrences): List[Occurrences] = {
      def expand(cf: (Char, Int)): Occurrences = {
        val seq = for {
          n <- 1 to cf._2
        } yield (cf._1, n)
        seq.toList
      }
      def expandOccurrences(occ: Occurrences): List[Occurrences] = {
        occ match {
          case Nil => List(List())
          case head :: tail =>
            for {
              x <- expand(head)
              y <- combinations1(tail)
            } yield x :: y
        }
      }
      expandOccurrences(occurrences)
    }
    val seqs = for {
      n <- 1 to occurrences.length
    } yield occurrences.combinations(n)
    seqs foreach(x => x foreach println)
    combinations1(occurrences)
  }                                               //> combinations: (occurrences: week6.anagrams.Occurrences)List[week6.anagrams.
                                                  //| Occurrences]

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
  combinations(abba) mkString ("\n")              //> List((a,2))
                                                  //| List((b,2))
                                                  //| List((a,2), (b,2))
                                                  //| res10: String = List((a,1), (b,1))
                                                  //| List((a,1), (b,2))
                                                  //| List((a,2), (b,1))
                                                  //| List((a,2), (b,2))
  abbacomb.toSet                                  //> res11: scala.collection.immutable.Set[List[(Char, Int)]] = Set(List((a,1)),
                                                  //|  List((b,1)), List((b,2)), List((a,1), (b,1)), List(), List((a,2), (b,1)), 
                                                  //| List((a,2)), List((a,1), (b,2)), List((a,2), (b,2)))
}