package streams

trait SolutionVisualizer extends GameDef with Solver with StringParserTerrain {
  lazy val levelExtens: Array[Int] =
    level.split("\n").map((str) => str.length())

  def displayStatus(b: Block): Unit = {
    print("Status with block at " + b)
    for (i <- 0 to levelExtens.size - 1; j <- 0 to levelExtens(i) - 1) {
      // start of the row
      if (j == 0) println();

      // Create the position
      val p = Pos(i, j)

      // Calculate the character
      val c: Char = if (b.b1 == p || b.b2 == p) '#'
      else if (terrain(p)) 'o'
      else '-'

      // Print the character
      print(c)

    }
    println()
  }

  def displaySolution(): Unit = {
    println("Visualizing solution found for game " + this.getClass().getName())
    displayStatus(solution.foldLeft(startBlock)((block, move) => {
      displayStatus(block);
      println("Performing move " + move)
      move match {
        case Left => { block.left }
        case Right => { block.right }
        case Up => { block.up }
        case Down => { block.down }
      }

    }))
    println("Game Over")
  }
}