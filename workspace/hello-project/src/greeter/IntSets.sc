package greeter

object IntSets {
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : greeter.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : greeter.IntSet = {.3{.4.}}
}