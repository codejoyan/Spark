package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInner(chars: List[Char], count: Int): Int = {
      if (chars.isEmpty || count < 0) count
      else if (chars.head == '(') balanceInner(chars.tail: List[Char], count + 1)
      else if (chars.head == ')') balanceInner(chars.tail: List[Char], count - 1)
      else balanceInner(chars.tail: List[Char], count)
    }
    val count = balanceInner(chars, 0)
    if (count >= 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def reduce(money: Int, coins: List[Int], acc: Int): Int = {
      if (money == 0) acc + 1
      else if (money < 0 || coins.isEmpty) acc
      else reduce(money - coins.head, coins, acc) + reduce(money, coins.tail: List[Int], acc)
    }
    if (money < 0 || coins.isEmpty) 0
    else reduce(money, coins, 0)
  }
}
