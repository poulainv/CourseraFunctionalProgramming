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
    if (c == 0 || r == 0) {
      1
    } else if (c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def check(chars: List[Char], nb: Integer): Boolean = {
      if (chars.isEmpty) {
        nb == 0
      } else if (nb < 0) {
        false
      } else if (chars.head.equals('(')) {
        check(chars.tail, nb + 1)
      } else if (chars.head.equals(')')) {
        check(chars.tail, nb - 1)
      } else {
        check(chars.tail, nb)
      }

    }
    check(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange2(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (money < 0) {
        0
      } else if (coins.isEmpty) {
        0
      } else {
        count(money - coins.head, coins) + count(money, coins.tail)
      }
    }
    count(money, coins)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, index: Int): Int = (money, index) match {
      case (0, _) => 1
      case (money, index) if (money < 0 || index > coins.length - 1) => 0
      case (money, index) if (money > 0) => count(money, index + 1) + count(money - coins(index), index)
    }
    count(money, 0)
  }
}
