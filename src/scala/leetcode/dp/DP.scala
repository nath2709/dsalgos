package leetcode.dp

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DP {

  def main(args: Array[String]): Unit = {
    //    print(fib(1))
    //        print(minCostClimbingStairs(Array(10,15,20)))
    //    print(climbStairs(3))
    //    println(maxSubArray(Array(5, 4, -1, 7, 8)))
    //    println(isSubsequence("bb", "aghbdc"))
    //    println("123".slice(0,1))
    //    "123".zipWithIndex.foreach(println)
    //    println(substrings("123"))
    //    println(numTeams(Array(2,5,3,4,1)))

    longestIncSubsequence(Array(50, 3, 10, 7, 40, 80))
  }

  def numTeams(rating: Array[Int]): Int = {

    var temp = 0
    var count = 0

    for (i <- rating.indices) {

      for (j <- i + 1 until rating.length) {

        for (k <- j + 1 until rating.length) {


          if (rating(i) < rating(j) && rating(j) < rating(k)) {

            count += 1
          }
          if (rating(i) < rating(j) && rating(j) < rating(k)) {
            count += 1
          }
        }
      }
    }
    count
  }

  //  TODO Count Sorted Vowel Strings
  def countSortedVowelString(n: Int): Int = {

    0
  }

  //TODO
  //  implement solution for longest subsequences
  def longestIncSubsequence(arr: Array[Int]): Int = {
    var max = 0

    for (i <- arr.indices) {
      //       println(arr(i))
      val maxArr: ListBuffer[Int] = ListBuffer.empty
      var temp = arr(i)
      maxArr.+=(temp)
      for (j <- i until arr.length) {

        if (temp < arr(j)) {
          println(arr(i), arr(j), temp, j, i)
          temp = arr(j)
          maxArr.+=(temp)

          //
        }
      }
      println(maxArr)
      println("-----------------------")

    }

    0
  }

  def substrings(n: String): Int = {
    val modulo = 1000000007
    //    val op = (0 until n.length).flatMap(idx => n.sliding(idx + 1, 1).map(_.toInt%modulo)).foldLeft(0){
    //      case(init,num)=> init+num%modulo
    //    }

    //    op%modulo
    var s = 0
    var prev_sum = 0
    n.zipWithIndex.foreach {
      case (n, idx) =>
        val s_ = prev_sum * 10 + (idx + 1) * n.toString.toInt
        s = s + s_
        prev_sum = s_
    }

    s % (modulo)

  }

  def fib(n: Int): Int = {
    if (n > 0) {
      val dp = Array.fill(n + 1)(0)
      dp(0) = 0
      dp(1) = 1
      for (i <- 2 until dp.length) yield (dp(i) = dp(i - 1) + dp(i - 2))
      dp(n)

    } else {
      0
    }

  }

  def minCostClimbingStairs(cost: Array[Int]): Int = {
    val arr = Array.fill(cost.length)(0)
    arr(0) = cost(0)
    arr(1) = cost(1)
    for (i <- 2 until cost.length) yield (arr(i) = cost(i) + Math.min(arr(i - 2), arr(i - 1)))
    Math.min(arr(cost.length - 1), arr(cost.length - 2))

  }

  def climbStairs(n: Int): Int = {
    val arr = Array.fill(n + 1)(0)
    arr(0) = 1
    arr(1) = 1
    for (i <- 2 to n) yield (arr(i) = arr(i - 1) + arr(i - 2))
    arr(n)
  }

  def maxSubArrayfunctional(nums: Array[Int]): Int = {
    (1 to nums.length).flatMap(index => {
      nums.sliding(index).map(arrays => arrays.sum)

    }).maxBy(x => x)
  }

  // Kadane's algorithms. idea is to first get the sum from starting, and compare it with max of (a[i]+sum)
  def maxSubArray(nums: Array[Int]): Int = {
    var curr = nums(0)
    var max = nums(0)
    for (i <- 1 until nums.length) {
      curr = Math.max(curr + nums(i), nums(i))
      max = Math.max(curr, max)
    }
    nums.foldLeft(0)((a, b) => b + a)
    nums.foldLeft(0, Int.MinValue) { case ((sum, max), num) =>
      val sum1 = Math.max(sum + num, num)
      val max1 = Math.max(max, sum1)
      (sum1, max1)
    }._2
    //    max
  }

  //392. Is Subsequence
  def isSubsequence(s: String, t: String): Boolean = {
    //  get all possible combinations of substrings and check if substring t is in it or not
    if (s.isEmpty) {
      return true
    }
    if (t.isEmpty || s.length > t.length) {
      return false
    }

    println(s, t)
    if (s(0) == t(0)) {
      isSubsequence(s.substring(1, s.length), t.substring(1, t.length))
    } else {
      isSubsequence(s, t.substring(1, t.length))
    }

  }

  def canCoinChange(coins: Array[Int], amount: Int, memo: collection.mutable.Map[Int, Boolean] = collection.mutable.Map.empty): Boolean = {

    if (memo.contains(amount)) {
      return memo(amount)
    }
    if (amount < 0) {
      return false

    }
    if (amount == 0) {
      return true
    }
    for (coin <- coins) {

      val temp = amount - coin
      if (canCoinChange(coins, temp, memo)) {
        memo(temp) = true
        return true
      }

    }
    memo(amount) = false
    return false
  }

  def howCoinChange(coins: Array[Int], amount: Int): List[Int] = {

    if (amount < 0) {
      return null

    }
    if (amount == 0) {
      return List()
    }
    for (coin <- coins) {

      val lst = howCoinChange(coins, amount - coin)
      if (lst != null) {
        return lst :+ coin
      }
    }

    return null
  }

  def coinChange(coins: Array[Int], amount: Int): Int = {

    def coinChange(coins: Array[Int], amount: Int, memo: collection.mutable.Map[Int, List[Int]] = collection.mutable.Map.empty): List[Int] = {

      println(amount, memo.mkString(","))
      //      memo.foreach(println)

      if (memo.contains(amount)) {

        return memo(amount)
      }
      if (amount == 0) {
        return List()
      }

      if (amount < 0) {
        return null
      }
      var shortest: List[Int] = null
      for (coin <- coins) {

        val temp = amount - coin
        val lst = coinChange(coins, temp, memo)
        if (lst != null) {
          var tempLst = lst :+ coin
          if (shortest == null || tempLst.length < shortest.length) {
            shortest = tempLst
          }
        }
      }
      memo(amount) = shortest
      shortest
    }

    if (coinChange(coins: Array[Int], amount: Int, collection.mutable.Map.empty) == null) {
      return -1
    }
    coinChange(coins: Array[Int], amount: Int, collection.mutable.Map.empty).length
  }

  def pairFriends(n: Int, temp: String): String = {


    if (n <= 1) {
      //      println(n)
      return temp + "," + n
      //      return lst
    }

    for (i <- 0 until n) {

      pairFriends(i, n + temp)
    }
    return temp
  }


  def combination(str: String, lst: mutable.HashSet[String]): mutable.HashSet[String] = {

    if (str.length <= 1) {
      lst.+=(str)
      //      println(str)
      return mutable.HashSet()
    }
    for (i <- 0 until str.length) {
      //            println(i,str.substring(0,i+1),str.substring(0,i)+str.substring(i+1,str.length))
      lst.+=(str.substring(0, i + 1))

      combination(str.substring(0, i) + str.substring(i + 1, str.length), lst)
      //      println(str.substring(0,i)+str.substring(i+1,str.length))
    }
    return lst
  }

  def allCombination(str: String, sfx: String, lst: mutable.ListBuffer[String]): ListBuffer[String] = {

    if (str.isEmpty) {
      lst.+=(sfx)
      return lst
      //      lst.+=(str)
      //      println(str)
      //      return mutable.HashSet()
    }
    for (i <- 0 until str.length) {

      //      println(i,sfx+str(i),str.substring(0,i)+str.substring(i+1,str.length))

      allCombination(str.substring(0, i) + str.substring(i + 1, str.length), sfx + str(i), lst)

      //      println(i, sfx,str.substring(0, i + 1), str.substring(i + 1, str.length))
      //      //      lst.+=(str.substring(0, i + 1))
      //
      //      allCombination(str.substring(0, i) + str.substring(i + 1, str.length), sfx + str.substring(0, i + 1) + str.substring(i + 1, str.length))
      //      //      println(str.substring(0,i)+str.substring(i+1,str.length))
    }
    return lst
  }


}
