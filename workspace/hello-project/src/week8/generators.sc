import week8._

object generators {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }                                               //> integers  : week8.Generator[Int] = generators$$anonfun$main$1$$anon$1@38dda2
                                                  //| 5b
  
  val booleans = integers.map(_ >= 0)             //> booleans  : week8.Generator[Boolean] = week8.Generator$$anon$1@77fddc31
  
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
  
  trees.generate                                  //> res0: week8.Tree = Inner(Inner(Inner(Leaf(1997092087),Inner(Inner(Leaf(16918
                                                  //| 8584),Inner(Leaf(1913909217),Inner(Leaf(-742074207),Inner(Inner(Leaf(-790742
                                                  //| 553),Inner(Inner(Leaf(-2092671042),Inner(Leaf(98204384),Inner(Inner(Leaf(-19
                                                  //| 08675144),Inner(Inner(Leaf(93154740),Inner(Inner(Inner(Leaf(-689034525),Leaf
                                                  //| (613291576)),Inner(Leaf(-445787589),Leaf(201016538))),Inner(Leaf(1460547307)
                                                  //| ,Leaf(109533068)))),Leaf(800585698))),Leaf(223997002)))),Leaf(1595692892))),
                                                  //| Leaf(1002050568))))),Inner(Inner(Leaf(329307123),Inner(Leaf(1094994257),Leaf
                                                  //| (852756500))),Inner(Leaf(2146354782),Leaf(148112558))))),Inner(Leaf(-1782851
                                                  //| 876),Leaf(-14261534))),Inner(Inner(Inner(Leaf(-1996054204),Inner(Inner(Leaf(
                                                  //| 696602830),Inner(Inner(Leaf(1992217114),Inner(Inner(Leaf(-1166970675),Leaf(1
                                                  //| 418360818)),Inner(Leaf(-1297507978),Leaf(607113777)))),Leaf(1027446072))),Le
                                                  //| af(1039648806))),Leaf(1042710157)),Inner(Inner(Inner(Inner(Leaf(-492883976),
                                                  //| Leaf(1263289244)),Inner(
                                                  //| Output exceeds cutoff limit.
}