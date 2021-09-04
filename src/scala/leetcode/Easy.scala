package leetcode

import scala.collection.mutable._

//import scala.collection.mutable

/**
 * Definition for singly-linked list.
 *
 */

object Easy {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  def isValid(s: String): Boolean = {

    var isvalid = false
    for (ch <- 0 until s.length() / 2) {

      val x = s.charAt(ch)
      val y = s.charAt(s.length() - 1 - ch)
      println(x + "\t" + y)
      if (x == '(' && y == ')' || x == '{' && y == '}' || x == '[' && y == ']') {
        isvalid = true
      }

    }

    isvalid
  }

  def longestCommonPrefix(strs: Array[String]): String = {

    var prefix = strs(0)
    for (str <- 1 until strs.length) {

      while (strs(str).indexOf(prefix) != 0) {

        prefix = prefix.substring(0, prefix.length() - 1)
      }

    }
    prefix
  }

  def maximum69Number(num: Int): Int = {

    var temp = num
    var place = 1
    var op = 0
    while (temp > 10) {
      temp = temp / 10
      place = place * 10
    }

    var temp1 = num
    var y = 0
    var count = 0
    while (place >= 1 && count == 0) {
      if (place == 0) place = 1
      y = temp1 % place
      temp1 = temp1 / place

      if (temp1 == 6) {
        temp1 = 9
        count = count + 1
      }
      op = op + temp1 * place
      place = place / 10
      temp1 = y
    }
    op + temp1
  }

  def maxProduct(nums: Array[Int]): Int = {

    var max1 = Integer.MIN_VALUE
    var max2 = Integer.MIN_VALUE
    var idx = 0
    for (i <- 0 until nums.length) {
      if (nums(i) > max1) {
        max2 = max1
        max1 = nums(i)
      } else if (nums(i) > max2) {
        max2 = nums(i)
      }
    }
    (max1 - 1) * (max2 - 1)
  }

  def freqAlphabets(s: String): String = {

    val map = Map(("1", "a"), ("2", "b"), ("3", "c"), ("4", "d"), ("5", "e"),
      ("6", "f"), ("7", "g"), ("8", "h"), ("9", "i"), ("10#", "j"),
      ("11#", "k"), ("12#", "l"), ("13#", "m"), ("14#", "n"),
      ("15#", "o"), ("16#", "p"), ("17#", "q"), ("18#", "r"),
      ("19#", "s"), ("20#", "t"), ("21#", "u"), ("22#", "v"),
      ("23#", "w"), ("24#", "x"), ("25#", "y"), ("26#", "z"))

    var temp = 0
    var str = ""
    var key = ""
    var x = s

    while (x.length() > 2) {

      if (x.charAt(temp + 2) == '#') {

        key = x.substring(temp, temp + 3)
        str = str + map.get(key).get
        x = x.substring(temp + 3)

      } else {
        key = x.substring(temp, 1)
        str = str + map.get(key).get
        x = x.substring(temp + 1)
      }
    }
    for (y <- 0 until x.length()) {
      str = str + map.get(x.charAt(y) + "").get
    }
    str
  }

  def reverseWords(s: String): String = {

    val words = s.split(" ")
    words.map(_.reverse).mkString(" ")
  }

  def subdomainVisits(cpdomains: Array[String]): List[String] = {

    val counts: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()

    val countDomains = cpdomains.toList.flatMap(item => {
      val elem = item.split(" ")
      val strs = elem(1).split("\\.")
      val x = for {
        i <- 0 until strs.length
        val op = elem(1).substring(elem(1).indexOf(strs(i)), elem(1).length())
      } yield op
      x.map(y => (y, elem(0).toInt)).toMap
    })
    println(countDomains)

    //    val x = cpdomains.toList.flatMap(item => {
    //      val elem = item.split(" ")
    //      val strs = elem(1).split("\\.").toList
    //      val op = strs.map(s => {
    //        val idx = elem(1).indexOf(s)
    //        val x = elem(1).substring(idx, elem(1).length())
    //      })
    ////      println(op)
    //      op
    //    })
    //    Nil
    //    val op = countDomains.foldLeft(Map[String, Int]())((counts, x) => {
    //
    //      if (counts.contains(x._1)) {
    //        counts + (x._1 -> (counts.get(x._1).get + x._2))
    //      } else {
    //        counts + (x._1 -> x._2)
    //      }
    //
    //    }).map { case (x, y) => y + " " + x }.toList

    //    op.foreach(println)
    Nil
  }

  def maxPower(s: String): Int = {

    if (s.isEmpty()) 0
    val (temp, next) = s.span(p => p == s.head)
    println(temp)
    println(next)

    //    s.toList.groupBy(x => x).map(x => (x._1, x._2.length)).foreach(println)
    //    def pack(ls: String): List[List[String]] = {
    //      if (ls.isEmpty) List(List())
    //      else {
    //        val (packed, next) = ls span { _ == ls.head }
    //        if (next == Nil) List(packed)
    //        else packed :: pack(next)
    //      }
    //    }
    //
    //    pack(s.toList).map(e => (e.length)).max
    0
  }

  def thousandSeparator(n: Int): String = {

    if (n < 1000) {
      return "" + n
    }

    val remainder = n % 1000
    val quotient = n / 1000
    var digits = "."
    if (remainder < 10) {
      digits = ".000"
    }
    if (remainder < 100) {
      println(remainder + "\t" + quotient)
      digits = ".0"
    }

    thousandSeparator(quotient) + digits + remainder
  }

  def removeDuplicates(S: String): String = {

    var i = 0
    var j = 1
    var temp = ""
    var s = S
    while (i < s.length() - 1) {

      if (s.charAt(i) != s.charAt(j)) {

        temp = temp + s.charAt(i)
        i = i + 1
        j = j + 1

      } else {
        i = i + 2
        j = j + 2
      }
      println(temp)
      s = s.substring(i, s.length())
      println(s)
    }
    ""
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    var map: Map[Int, Int] = Map()
    var array: ArrayBuffer[Int] = ArrayBuffer()
    for (i <- 0 until nums.length) {
      val temp = target - nums(i)
      if (map.contains(nums(i))) {
        array.+=(i).+=(map(nums(i)))

      }
      map = map.+=(temp -> i)
    }
    array.toArray

  }

  def addBinary(a: String, b: String): String = {

    val x = a.toInt
    val y = b.toInt
    var sum_a = 0
    var sum_b = 0
    var pow = 0

    for (i <- a.length() - 1 to 0 by -1) {
      sum_a = sum_a + (a.charAt(i).asDigit * Math.pow(2, pow).toInt)
      pow = pow + 1
    }

    pow = 0
    for (i <- b.length() - 1 to 0 by -1) {
      sum_b = sum_b + (b.charAt(i).asDigit * Math.pow(2, pow).toInt)
      pow = pow + 1
    }
    println(sum_a, sum_b)
    var temp = sum_a + sum_b
    var remain = 0
    var op = ""
    while (temp > 1) {

      remain = temp % 2
      temp = temp / 2

      op = remain + op
    }
    op = temp + op

    op
  }

  def addDigits(num: Int): Int = {
    if (num == 0)
      return num
    val temp = num % 9

    if (temp == 0)
      9
    else {
      num % 9
    }

  }

  def addStrings(num1: String, num2: String): String = {

    var sum = ""
    var carry = 0
    var counter1 = 0
    var counter2 = 0
    while (num1.length() > counter1 || num2.length() > counter2) {

      var ch1 = 0
      if (num1.length() - 1 - counter1 >= 0) {
        ch1 = num1.charAt(num1.length() - 1 - counter1) - 48
      }
      var ch2 = 0
      if (num2.length() - 1 - counter2 >= 0) {
        ch2 = num2.charAt(num2.length() - 1 - counter2) - 48
      }

      sum = (carry + ch1 + ch2) % 10 + sum
      carry = (carry + ch1 + ch2) / 10

      counter1 = counter1 + 1
      counter2 = counter2 + 1
    }

    if (carry == 0) {
      sum
    } else {
      carry + sum
    }

  }

  def detectCycle(head: ListNode): ListNode = {
    var slowNode = head
    var fastNode = head.next.next
    while (slowNode != null && fastNode != null && fastNode.next != null) {
      slowNode = slowNode.next
      fastNode = fastNode.next.next
      if (slowNode == fastNode) {
        return slowNode
      }
    }
    return null
  }

  def addToArrayForm(A: Array[Int], K: Int): List[Int] = {

    val op = A.foldRight((0l, 0l)) {
      case (x, (sum, power)) =>

        val y = x * math.pow(10, power).toInt
        (sum + y, power + 1)

    }
    println(op)
    println(op._1 + K)

    op.toString().map(x => x.asDigit).toList

  }

  def numtobinary(num: Int): String = {

    def numTobinary(num: Int, remain: String): String = {

      if (num <= 1) {
        num + remain
      } else {
        println(num / 2, remain, num % 2)
        numTobinary(num / 2, remain + "" + num % 2)
      }

    }

    numTobinary(num, "")
  }

  def backspaceCompare(S: String, T: String): Boolean = {

    def deleteChar(S: String): String = {

      S.foldLeft(new StringBuilder) {
        case (sb, ch) =>
          if (ch == '#') {
            sb.append(ch)
          } else {
            if (!sb.isEmpty) {
              sb.deleteCharAt(sb.length - 1)
            }
            sb
          }
      }.toString()

    }

    deleteChar(S) == deleteChar(T)
  }

  def reverseVowels(s: String): String = {

    var i = 0
    var j = s.length() - 1
    var temp = s.toCharArray()

    while (i < j) {

      var chi = temp(i)
      var chj = temp(j)
      if (!isVowel(chi)) {
        i = i + 1

      }
      if (!isVowel(chj)) {
        j = j - 1

      }
      if (isVowel(chi) && isVowel(chj)) {
        var z = chi
        temp(i) = chj
        temp(j) = z

        i = i + 1
        j = j - 1
      }

    }
    temp.mkString("")
  }

  private def isVowel(ch: Char): Boolean = {

    ch match {
      case 'a' => true
      case 'e' => true
      case 'i' => true
      case 'o' => true
      case 'u' => true
      case 'A' => true
      case 'E' => true
      case 'I' => true
      case 'O' => true
      case 'U' => true
      case _ => false
    }

  }

  def isLongPressedName(name: String, typed: String): Boolean = {

    val arr = typed.toCharArray()

    val typedCount = arr.foldLeft(collection.immutable.Map[Char, Int]()) {
      case (map, ch) => {
        if (map.contains(ch)) {
          val count = map.get(ch).get + 1
          map.+(ch -> count)
        } else {
          map.+(ch -> 1)
        }

      }
    }
    val nameCount = name.toCharArray().foldLeft(collection.immutable.Map[Char, Int]()) {
      case (map, ch) => {
        if (map.contains(ch)) {
          val count = map.get(ch).get + 1
          map.+(ch -> count)
        } else {
          map.+(ch -> 1)
        }

      }
    }

    val contains = nameCount.map(ch => {
      val tCount = ch._2

      val count = typedCount.get(ch._1).get

      if (tCount >= count) {
        true
      } else {
        false
      }

    })

    println(contains)
    !contains.find(x => x.==(false)).isDefined

    //    val temp = List()
    //
    //    while (i < typed.length() - 1) {
    //      //      println(i, j)
    //      val chi = arr(i)
    //      val chj = arr(j)
    //
    //      if (chi == chj) {
    //        s = s + arr(j)
    //        i = i + 2
    //        j = j + 2
    //
    //      } else {
    //        s = s + arr(i)
    //        i = i + 1
    //        j = j + 1
    //
    //      }
    //
    //    }
    //
    //    println(s, s.length(), name.length(), typed.length())
    //
    //    if (s.length() >= name.length()) {
    //      s = s.substring(0, name.length())
    //    } else {
    //      s = s + typed(typed.length() - 1)
    //    }
    //    //    if (j == typed.length()) {
    //    //      s = s + typed(typed.length() - 1)
    //    //    }
    //
    //    println(s)
    //    s.equals(name)

  }

  def strStr(haystack: String, needle: String): Int = {

    haystack.sliding(needle.length()).zipWithIndex.collectFirst({ case (`needle`, y) => y }).getOrElse(-1)

  }

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {

    var idx = m - 1
    var idx1 = n - 1

    for (i <- m + n - 1 to 0 by -1) {
      var temp = 0
      var temp1 = 0

      if (idx < 0) {
        nums1(i) = nums2(idx1)
        idx1 = idx1 - 1
      } else if (idx1 < 0 || nums1(idx) > nums2(idx1)) {
        nums1(i) = nums1(idx)
        idx = idx - 1
      } else {
        nums1(i) = nums2(idx1)
        idx1 = idx1 - 1
      }
    }

  }

  def countLargestGroup(n: Int): Int = {

    def sumdigits(num: Int, sum: Int): Int = {

      num < 10 match {
        case true => sum + num
        case false => sumdigits(num / 10, sum + num % 10)
      }
    }

    val count = 1.to(n).toArray.map(x => sumdigits(x, 0)).groupBy(identity).mapValues(_.size).map(_._2).max
    1.to(n).toArray.map(x => sumdigits(x, 0)).groupBy(identity).mapValues(_.size).count(x => x._2 == count)

  }

  def findLucky(arr: Array[Int]): Int = {

    val filtered = arr.groupBy(identity).mapValues(_.length).filter(ele => ele._1 == ele._2)
    filtered.isEmpty match {
      case true => -1
      case false => filtered.map(_._1).max
    }

  }

  def largestAltitude(gain: Array[Int]): Int = {

    gain.zipWithIndex.map { case (ele, idx) => gain.slice(0, idx + 1).+:(0) }.map(_.sum).+:(0).max

  }

  def main(args: Array[String]): Unit = {

    val gains = Array(-4, -3, -2, -1, 4, 3, 2)
    println(largestAltitude(gains))


    //    val op = findLucky(Array(2, 2, 2, 3, 3))
    //    println(op)

    //    val test = "hello"
    //    val test1 = "helloworld"
    //    println(test1.indexOf(test))
    //    println(longestCommonPrefix(Array("dog","racecar","car")))
    //    println(isValid("(){}}{"))
    //    println(maximum69Number(9669))
    //    println(maxProduct(Array(1, 5, 4, 5)))

    //    val op = freqAlphabets("12345678910#11#12#13#14#15#16#17#18#19#20#21#22#23#24#25#26#")
    //    val op = reverseWords("Let's take LeetCode contest")
    //    println(op)
    //    val op = subdomainVisits(Array("900 google.mail.com", "50 yahoo.com", "1 intel.mail.com", "5 wiki.org"))
    //    val op = maxPower("bbcccddddeeeeedcba")
    //    println(op)

    //    val op = thousandSeparator(1234)
    //    val op = removeDuplicates("abbaca")
    //    println(op)
    //    val op = List("a", "a", "b", "b", "c")
    //    op.groupBy(x => x).map(x => (x._1,x._2.length)).foreach(println)
    //    val op = twoSum(Array(2, 7, 11, 15), 9)
    //    op.foreach(println)

    //    val num = 4
    //    val num1 = 6
    //    if (num == 5) {
    //      print("yes")
    //    } else if (num1 >= 5) {
    //      println("gyes")
    //    } else {
    //      println("no")
    //    }
    //    println(num/2,num%2)
    //    val op = addBinary("11", "1")
    //    println(op)
    //    val op = addDigits(0)

    //    val op = addStrings("12", "1")
    //    println(op)

    //    val op = addToArrayForm(Array(9, 9, 9, 9, 9, 9, 9, 9, 9, 9), 1)
    //    println(op)
    //    println(backspaceCompare("ab#c", "ad#c"))

    //     println(isVowel('s'))

    //    println(reverseVowels("aA"))
    //    println(isLongPressedName(name = "saeed", typed = "ssaaedd"))
    //        println(isLongPressedName(name = "vtkgn", "vttkgnn"))
    //    println(isLongPressedName("abcd", "aaabbbcccddd"))
    //    println(isLongPressedName("xnhtq", "xhhttqq"))
    //    strStr(haystack = "hello", needle = "ll")

    //    val nums1 = Array(-1, 0, 0, 3, 3, 3, 0, 0, 0)
    //    val m = 6
    //    val nums2 = Array(1, 2, 2)
    //    val n = 3
    //    val nums2: Array[Int] = Array()
    //    val m = 1
    //    val n = 0
    //    merge(nums1, m, nums2, n)
    //    nums1.foreach(println)
    //    countLargestGroup(13)
  }

}