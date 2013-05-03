package week6

object anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  val dictionary: List[Word] = loadDictionary     //> Classes dir: C:\Users\gdolgich\dev\scala-course\workspace\hello-project\bin\
                                                  //| week6
                                                  //| Project dir: C:\Users\gdolgich\dev\scala-course
                                                  //| Resource file: C:\Users\gdolgich\dev\scala-course\workspace\hello-project\sr
                                                  //| c\main\resources\forcomp\linuxwords.txt
                                                  //| dictionary  : List[week6.anagrams.Word] = List(Aarhus, Aaron, Ababa, aback, 
                                                  //| abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased,
                                                  //|  abasement, abasements, abases, abash, abashed, abashes, abashing, abasing, 
                                                  //| abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, a
                                                  //| bbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, a
                                                  //| bbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal
                                                  //| , abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe
                                                  //| , abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, a
                                                  //| berrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorr
  val word = "Robert"                             //> word  : String = Robert
  
  val grouped = word.toLowerCase.groupBy(x => x)  //> grouped  : scala.collection.immutable.Map[Char,String] = Map(e -> e, t -> t,
                                                  //|  b -> b, r -> rr, o -> o)
  val mapped = grouped.mapValues(x => x.length)   //> mapped  : scala.collection.immutable.Map[Char,Int] = Map(e -> 1, t -> 1, b -
                                                  //| > 1, r -> 2, o -> 1)
  val occ = mapped.toList.sorted                  //> occ  : List[(Char, Int)] = List((b,1), (e,1), (o,1), (r,2), (t,1))
}