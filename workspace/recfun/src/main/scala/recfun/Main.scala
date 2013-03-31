package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(level: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) level
      else if (level < 0) level
      else if (chars.head == '(') loop(level + 1, chars.tail)
      else if (chars.head == ')') loop(level - 1, chars.tail)
      else loop(level, chars.tail)
    }
    loop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
