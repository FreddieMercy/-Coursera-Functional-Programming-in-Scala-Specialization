package reductions

import org.scalameter.*

import scala.annotation.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface :

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance_helper(left: Int, right: Int, index: Int): Boolean = {

      if (index >= chars.length) {
        return left == right
      }

      if (right > left) {
        return false
      }

      if (chars(index) == '(') {
        if (right > 0) {
          return balance_helper(left, right - 1, index + 1)
        }
        else {
          return balance_helper(left + 1, right, index + 1)
        }
      }

      if (chars(index) == ')') {
        if (left > 0) {
          return balance_helper(left - 1, right, index + 1)
        }
        else {
          return balance_helper(left, right + 1, index + 1)
        }
      }

      return balance_helper(left, right, index + 1)
    }

    balance_helper(0, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) = {
      if (idx >= until) {
        return (left, right)
      }

      chars(idx) match
        case '(' =>
          if (right > 0) {
            traverse(idx + 1, until, left, right - 1)
          }
          else {
            traverse(idx + 1, until, left + 1, right)
          }
        case ')' =>
          if (left > 0) {
            traverse(idx + 1, until, left - 1, right)
          }
          else {
            traverse(idx + 1, until, left, right + 1)
          }
        case _ => traverse(idx + 1, until, left, right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        return traverse(from, until, 0, 0)
      }

      val mid: Int = (until - from) / 2 + from
      val ((left_open: Int, left_closed: Int), (right_open: Int, right_closed: Int)) = parallel(reduce(from, mid), reduce(mid, until))

      if (left_open >= right_closed) {
        (right_open + left_open - right_closed, left_closed)
      }
      else {
        (right_open, left_closed + right_closed - left_open)
      }
    }

    reduce(0, chars.length) == (0, 0)

  }

