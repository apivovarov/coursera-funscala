package recfun

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
    if (c == 0 || r == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCnt(chars: List[Char], openCnt: Int): Int = {
      if (chars.isEmpty || openCnt < 0) {
        openCnt
      } else {
        val head = chars.head
        val tail = chars.tail
        val openCnt2 = if (head == '(') {
          openCnt + 1
        } else if (head == ')') {
          openCnt - 1
        } else {
          openCnt
        }
        balanceCnt(tail, openCnt2)
      }
    }
    balanceCnt(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeInter(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else countChangeInter(money - coins.head, coins) + countChangeInter(money, coins.tail)
    }
    if (money == 0) 0
    else countChangeInter(money, coins)
  }
}
