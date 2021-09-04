package leetcode

import scala.collection.mutable.Map

object LongestSubstring {

  def longestSubstring(str: String): Unit = {

    var max = 0
    for (x <- 0 until str.length()) {
      var temp = ""
      //      temp = temp + str.charAt(x)
      for (y <- x until str.length()) {

        if (!temp.contains(str.charAt(y))) {
          temp = temp + str.charAt(y)
          //          println(temp)
        } else {
          temp = str.charAt(y) + ""
        }
        if (temp.length() > max) max = temp.length()

      }
    }
    println(max)
  }

  def longestSubstringMap(str: String): Unit = {

    val temp: Map[String, String] = Map()
    for (x <- 0 until str.length()) {
      //      temp = temp + str.charAt(x)
      for (y <- x until str.length()) {

        var a = str.charAt(y) + ""

        if (!temp.contains(a)) {
          temp.put(a, a)
        } else {

        }
      }
    }
  }

//  def runningSum(nums: Array[Int]): Array[Int] = {
//
//    nums.foldLeft(Array[Int]())((x, y) => x + y)
//  }

  def main(args: Array[String]): Unit = {

    longestSubstring("pwwke")
  }
}