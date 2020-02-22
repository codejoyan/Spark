package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
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
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def reduction(chars: Array[Char], idx: Int, count: Int): Boolean = {
      if ((count < 0) || (idx == chars.length && count != 0)) false
      else if (idx == chars.length && count == 0) true
      else if (chars(idx) == '(') reduction(chars, idx + 1, count + 1)
      else if (chars(idx) == ')') reduction(chars, idx + 1, count - 1)
      else reduction(chars, idx + 1, count)
    }
    reduction(chars, 0, 0)
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(from: Int, until: Int, idx: Int, count: Int): Int = {
      if (from == 0 && count < 0) Integer.MIN_VALUE
      else if (idx > until) count
      else if (chars(idx) == '(') traverse(from, until, idx + 1, count + 1)
      else if (chars(idx) == ')') traverse(from, until, idx + 1, count - 1)
      else traverse(from, until, idx + 1, count)
    }

    def reduce(from: Int, until: Int): Int = {
      
      if ((until - from) <= threshold) {
        //println("from:" + from + "until:" + until + "return:"+ traverse(from, until, from, 0))
        traverse(from, until, from, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val (t1, t2) = parallel(reduce(from, mid), reduce(mid + 1, until))
        t1 + t2
      }
    }
    reduce(0, chars.length-1) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
