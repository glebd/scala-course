import week8._

object generators {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }                                               //> integers  : week8.Generator[Int] = generators$$anonfun$main$1$$anon$1@745b06
                                                  //| 76
  
  val booleans = integers.map(_ >= 0)             //> booleans  : week8.Generator[Boolean] = week8.Generator$$anon$1@48fa2a5c
  
  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)                                 //> leafs: => week8.Generator[week8.Leaf]
  
  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)                             //> inners: => week8.Generator[week8.Inner]
  
  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree                                    //> trees: => week8.Generator[week8.Tree]
  
  trees.generate                                  //> res0: week8.Tree = Inner(Leaf(-599420715),Inner(Inner(Inner(Inner(Leaf(15736
                                                  //| 40829),Inner(Leaf(2074102918),Leaf(-682509464))),Leaf(1167507232)),Leaf(2753
                                                  //| 28633)),Inner(Leaf(-695716780),Leaf(-2051988067))))
}