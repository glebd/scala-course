package week8

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val f: PartialFunction[String, String] = {case "ping" => "pong"}
                                                  //> f  : PartialFunction[String,String] = <function1>
  f("ping")                                       //> res0: String = pong
  f.isDefinedAt("abc")                            //> res1: Boolean = false
  f.isDefinedAt("ping")                           //> res2: Boolean = true
}