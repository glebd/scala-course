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
  
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(x => x).mapValues(_.length).toList.sorted
                                                  //> wordOccurrences: (w: week6.anagrams.Word)week6.anagrams.Occurrences
  
  val dictOcc = dictionary.map(w => (w, wordOccurrences(w)))
                                                  //> dictOcc  : List[(week6.anagrams.Word, week6.anagrams.Occurrences)] = List((A
                                                  //| arhus,List((a,2), (h,1), (r,1), (s,1), (u,1))), (Aaron,List((a,2), (n,1), (o
                                                  //| ,1), (r,1))), (Ababa,List((a,3), (b,2))), (aback,List((a,2), (b,1), (c,1), (
                                                  //| k,1))), (abaft,List((a,2), (b,1), (f,1), (t,1))), (abandon,List((a,2), (b,1)
                                                  //| , (d,1), (n,2), (o,1))), (abandoned,List((a,2), (b,1), (d,2), (e,1), (n,2), 
                                                  //| (o,1))), (abandoning,List((a,2), (b,1), (d,1), (g,1), (i,1), (n,3), (o,1))),
                                                  //|  (abandonment,List((a,2), (b,1), (d,1), (e,1), (m,1), (n,3), (o,1), (t,1))),
                                                  //|  (abandons,List((a,2), (b,1), (d,1), (n,2), (o,1), (s,1))), (abase,List((a,2
                                                  //| ), (b,1), (e,1), (s,1))), (abased,List((a,2), (b,1), (d,1), (e,1), (s,1))), 
                                                  //| (abasement,List((a,2), (b,1), (e,2), (m,1), (n,1), (s,1), (t,1))), (abasemen
                                                  //| ts,List((a,2), (b,1), (e,2), (m,1), (n,1), (s,2), (t,1))), (abases,List((a,2
                                                  //| ), (b,1), (e,1), (s,2))), (abash,List((a,2), (b,1), (h,1), (s,1))), (abashed
                                                  //| ,List((a,2), (b,1), (d,1
                                                  //| Output exceeds cutoff limit.
  
  val dictOccGrouped = dictOcc.groupBy(x => x._2) //> dictOccGrouped  : scala.collection.immutable.Map[week6.anagrams.Occurrences,
                                                  //| List[(week6.anagrams.Word, week6.anagrams.Occurrences)]] = Map(List((e,1), (
                                                  //| i,1), (l,1), (r,1), (t,2)) -> List((litter,List((e,1), (i,1), (l,1), (r,1), 
                                                  //| (t,2)))), List((a,1), (d,1), (e,1), (g,2), (l,1), (r,1)) -> List((gargled,Li
                                                  //| st((a,1), (d,1), (e,1), (g,2), (l,1), (r,1)))), List((a,1), (e,1), (h,1), (i
                                                  //| ,1), (k,1), (n,1), (s,3)) -> List((shakiness,List((a,1), (e,1), (h,1), (i,1)
                                                  //| , (k,1), (n,1), (s,3)))), List((e,2), (g,1), (n,1)) -> List((gene,List((e,2)
                                                  //| , (g,1), (n,1)))), List((a,2), (n,1), (t,1), (y,1)) -> List((Tanya,List((a,2
                                                  //| ), (n,1), (t,1), (y,1)))), List((a,1), (d,1), (e,2), (h,1), (m,1), (n,2), (o
                                                  //| ,1), (s,3)) -> List((handsomeness,List((a,1), (d,1), (e,2), (h,1), (m,1), (n
                                                  //| ,2), (o,1), (s,3)))), List((a,2), (c,1), (e,2), (k,1), (l,1), (m,1), (p,1), 
                                                  //| (r,1), (t,1)) -> List((marketplace,List((a,2), (c,1), (e,2), (k,1), (l,1), (
                                                  //| m,1), (p,1), (r,1), (t,1
                                                  //| Output exceeds cutoff limit.
  
  val dictOccGroupMap = dictOccGrouped.map(x => (x._1, x._2.map(y => y._1)))
                                                  //> dictOccGroupMap  : scala.collection.immutable.Map[week6.anagrams.Occurrences
                                                  //| ,List[week6.anagrams.Word]] = Map(List((e,1), (i,1), (l,1), (r,1), (t,2)) ->
                                                  //|  List(litter), List((a,1), (d,1), (e,1), (g,2), (l,1), (r,1)) -> List(gargle
                                                  //| d), List((a,1), (e,1), (h,1), (i,1), (k,1), (n,1), (s,3)) -> List(shakiness)
                                                  //| , List((e,2), (g,1), (n,1)) -> List(gene), List((a,2), (n,1), (t,1), (y,1)) 
                                                  //| -> List(Tanya), List((a,1), (d,1), (e,2), (h,1), (m,1), (n,2), (o,1), (s,3))
                                                  //|  -> List(handsomeness), List((a,2), (c,1), (e,2), (k,1), (l,1), (m,1), (p,1)
                                                  //| , (r,1), (t,1)) -> List(marketplace), List((a,1), (i,1), (l,2), (s,1), (v,1)
                                                  //| ) -> List(villas), List((d,2), (e,1), (h,2), (n,1), (r,1), (t,1), (u,1)) -> 
                                                  //| List(hundredth), List((a,3), (b,1), (c,1), (h,1), (i,2), (l,1), (o,1), (p,2)
                                                  //| , (r,1), (t,1), (y,1)) -> List(approachability), List((d,1), (e,2), (l,1), (
                                                  //| s,1), (t,2)) -> List(settled), List((a,1), (g,1), (i,3), (l,1), (n,2), (t,1)
                                                  //| , (z,1)) -> List(Latiniz
                                                  //| Output exceeds cutoff limit.
}