import scala.collection.immutable.List

object Practice {

  def main(args: Array[String]): Unit = {

    //    binqueries()
    //    val input = 12
    //
    //    val temp = List(1, 2)
    //
    //    val op = temp.filter(x => x != 0 && input % x == 0).length
    //    print(op)

    //    val x = find(input, List())
    //
    //    x.reduce(t => {
    //      if(input%t==0) 1
    //
    //    })

    //    println(111 / 10)
    //    println(111 % 10)
    //    println(reverse(List(1, 2, 3, 4), List()))

    //    val map = Map(("a" -> 1), ("b" -> 1))
    //
    //    val x = "a"
    //    if (map.contains(x)) {
    //      val key = x+ (map(key) + 1))
    //    } else {
    //      val y = map + (x -> 1)
    //      y
    //    }
    //
    //    op.foreach(println)

  }

  def reverse(ls: List[Int], temp: List[Int]): List[Int] = ls match {

    case Nil => {
      println(ls)
      temp
    }
    case head :: tail => {
      println(ls)
      reverse(tail, temp ::: tail.+:(head))
    }

  }

  def binqueries() = {
    val name = scala.io.StdIn.readLine() // Reading input from STDIN
    //   sa

  }

  //  def findDigits(n: Int): Int = {
  //
  //    find(111, List())
  //  }

  def find(n: Int, digits: List[Int]): List[Int] = {

    //    println(s"${digits.mkString(",")}")
    if (n < 10) {
      return digits.::(n)
      //      digits.foreach(print)

    }

    val t = n / 10
    val r = n % 10

    find(t, digits.::(r))

  }

  def sum(l1: List[Int], l2: List[Int]) = {

  }

}