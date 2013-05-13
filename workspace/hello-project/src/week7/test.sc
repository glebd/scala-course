package week7

object test {
  val problem = new Pouring(Vector(4, 9, 19))     //> problem  : week7.Pouring = week7.Pouring@4d14ca44
  problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with we
                                                  //| ek7.test.problem.Move] = Vector(Empty(0), Empty(1), Empty(2), Fill(0), Fill(1
                                                  //| ), Fill(2), Pour(0,1), Pour(0,2), Pour(1,0), Pour(1,2), Pour(2,0), Pour(2,1))
                                                  //| 
  
  problem.pathSets.take(3).toList                 //> res1: List[Set[week7.test.problem.Path]] = List(Set(--> Vector(0, 0, 0)), Se
                                                  //| t(Fill(0)--> Vector(4, 0, 0), Fill(1)--> Vector(0, 9, 0), Fill(2)--> Vector(
                                                  //| 0, 0, 19)), Set(Fill(2) Pour(2,0)--> Vector(4, 0, 15), Fill(1) Pour(1,0)--> 
                                                  //| Vector(4, 5, 0), Fill(2) Pour(2,1)--> Vector(0, 9, 10), Fill(0) Fill(1)--> V
                                                  //| ector(4, 9, 0), Fill(2) Fill(1)--> Vector(0, 9, 19), Fill(1) Fill(2)--> Vect
                                                  //| or(0, 9, 19), Fill(1) Pour(1,2)--> Vector(0, 0, 9), Fill(2) Fill(0)--> Vecto
                                                  //| r(4, 0, 19), Fill(0) Pour(0,1)--> Vector(0, 4, 0), Fill(0) Fill(2)--> Vector
                                                  //| (4, 0, 19), Fill(1) Fill(0)--> Vector(4, 9, 0), Fill(0) Pour(0,2)--> Vector(
                                                  //| 0, 0, 4)))
  problem.solutions(6)                            //> res2: Stream[week7.test.problem.Path] = Stream(Fill(2) Pour(2,1) Pour(2,0)--
                                                  //| > Vector(4, 9, 6), ?)
}