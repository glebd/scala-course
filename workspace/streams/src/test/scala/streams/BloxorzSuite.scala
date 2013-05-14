package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level0 extends SolutionChecker {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
    val optsolution = List(Down, Right, Up)
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level6 extends SolutionChecker {
    val level =
      """-----oooooo
      |-----o--ooo
      |-----o--ooooo
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo
      |------ooooo
      |------ooooo
      |-------ooo""".stripMargin

    val optsolution = ???
  }

  test("terrain 0 negative") {
    new Level0 {
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
      assert(!terrain(Pos(-1, -1)), "-1,-1")
    }
  }

  test("terrain 1 negative") {
    new Level1 {
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
      assert(!terrain(Pos(-1, -1)), "-1,-1")
    }
  }

  test("terrain 0 out of bounds") {
    new Level0 {
      assert(!terrain(Pos(1, 1)), "1,1")
      assert(!terrain(Pos(1, 2)), "1,2")
      assert(!terrain(Pos(4, 4)), "4,4")
    }
  }

  test("terrain 1 out of bounds") {
    new Level1 {
      assert(!terrain(Pos(0, 10)), "0,10")
      assert(!terrain(Pos(5, 0)), "5,0")
      assert(!terrain(Pos(8, 12)), "8,12")
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }
  
  test("neighborsWithHistory level 1") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet ===
        Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("newNeighborsWithHistory level 1") {
    new Level1 {
      val newNeighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      assert(newNeighbors === Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }
  
  test("optimal solution for level 0") {
    new Level0 {
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 0") {
    new Level0 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
