package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1,3)
    val goal = Pos(5,8)
  }
  println("Infinite Level")
  println(InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain 
   */
  abstract class Level extends Solver with StringParserTerrain
  
  object Level0 extends SolutionVisualizer {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println("\nLevel 0")
  println(Level0.solution)
  Level0.displaySolution()

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends SolutionVisualizer {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println("\nLevel 1")
  println(Level1.solution)
  Level1.displaySolution()
}
