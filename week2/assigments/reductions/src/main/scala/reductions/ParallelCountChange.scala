package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      if (threshold(money, coins)) {
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
      } else {
        val par = parallel(parCountChange(money, coins.tail, threshold), parCountChange(money - coins.head, coins, threshold))
        par._1 + par._2
      }
    }
  }

  /** Threshold heuristic based on the starting money.
    * This function creates a threshold that returns true when
    * the amount of money is less than or equal to 2 / 3 of the starting amount.
    */
  def moneyThreshold(startingMoney: Int): Threshold = { (currentMoney, _) =>
    val eval = (startingMoney * 2) / 3
    currentMoney <= eval
  }

  /** Threshold heuristic based on the total number of initial coins.
    * Implement the method totalCoinsThreshold, which
    * returns a threshold function that returns true when the number of coins is less than or
    * equal to the 2 / 3 of the initial number of coins.
    */
  def totalCoinsThreshold(totalCoins: Int): Threshold = { (_, coins) =>
    coins.size <= (totalCoins * 2) / 3
  }

  /** Threshold heuristic based on the starting money and the initial list of coins.
    * Implement the method combinedThreshold, which returns a threshold
    * function that returns true when the amount of money multiplied with the number of
    * remaining coins is less than or equal to the starting money multiplied with the initial
    * number of coins divided by 2
    *
    */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = { (currentMoney, coins) =>
    val eval = currentMoney * coins.size
    eval <= startingMoney * (allCoins.size / 2)
  }
}
