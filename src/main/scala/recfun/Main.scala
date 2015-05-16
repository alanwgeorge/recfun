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
    if (c == 0 || c == r ) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], numOpen: Int): Boolean = {
      if (chars.length == 0 && numOpen == 0) true
      else if (chars.length == 0 && numOpen > 0) false
      else if (numOpen < 0) false
      else
        if (chars.head == '(') balance(chars.tail, numOpen + 1)
        else if (chars.head == ')') balance(chars.tail, numOpen - 1)
        else balance(chars.tail, numOpen)
    }

    balance(chars, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins.sorted
    def count(money: Int, coins: List[Int], count: Int) :Int = {
      println(coins.head)
      if (money == 0) count + 1
      else money
    }

    if (money <= 0 || coins.isEmpty) 0
    else count(money, coins, 0)
  }
}