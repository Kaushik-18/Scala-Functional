package recfun

import math.abs

object Main {
  def main(args: Array[String]) {
    // countChange(4, List(1, 2))
    /*for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }*/

    def anon_example(y: Int) = (x: Int) => x * y

    // def fact(n: Int) = product(x => x)(1, n)
    // fact(3)
    // def combofunction(f: Int => Int)(breakvalue: Int) = prodsum(f, breakvalue, (x, y) => x + y)(1, 3)

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
    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }
    balanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def helper(amount: Int, coins: List[Int], nOfCoins: Int): Int = {
      if (amount < 0) return 0
      if (amount == 0) return 1
      if (nOfCoins <= 0 && amount >= 1) return 0

      helper(amount, coins, nOfCoins - 1) +
        helper(amount - coins(nOfCoins - 1), coins, nOfCoins)
    }
    helper(money, coins, coins.length)
  }

  def sum(p: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else
      p(a) + sum(p, a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else
      f(a) * product(f)(a + 1, b)
  }

  def prodsum(f: Int => Int, breakvalue: Int, combiner: (Int, Int) => Int)(a: Int, b: Int): Int =
    if (a > b) breakvalue
    else
      combiner(f(a), prodsum(f, breakvalue, combiner)(a + 1, b))


  def estimaton(x: Double, y: Double) =
    abs((x - y) / y) / x < 0.01

  def calculateRoot(f: Double => Double)(x: Double) = {
    def iters(x: Double): Double = {
      val next = f(x)
      if (estimaton(x, next)) next
      else iters(next)
    }
    iters(x)
  }

  def sqrtest(x: Double) = calculateRoot(y => y + x / y)(1)

}

