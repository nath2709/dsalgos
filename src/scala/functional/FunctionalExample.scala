package functional

object FunctionalExample {

  def identity[A](ele: A): A = {
    ele
  }

  def reverser(str: String): String = {
    str.reverse
  }

  def safeStringOps(str: String, f: String => String): String = {
    if (str != null) f(str) else str
  }

  //  List Replication
  def f_1(num: Int, arr: List[Int]): List[Int] = {

    arr.flatMap(ele => List.fill(num)(ele))
    //    List()
  }

  //  Filter Positions in a List
  def f_2(arr: List[Int]): List[Int] = {
    val x = for {
      i <- arr.indices
      if (i % 2 != 0)
    } yield arr(i)
    x.toList
  }

  // reverse
  def f_3(arr: List[Int]): List[Int] = {
    arr match {
      case head :: Nil => head :: List()
      case head :: tail => f_3(tail) :+ head
    }
  }

  //  sum odd number
  def f(arr: List[Int]): Int = {

    arr.filter(_ % 2 != 0).sum

  }
  def main(args: Array[String]): Unit = {

    val temp = List(3,2,4,6,5,7,8,0,1)
    println(f(temp))
    //    List.fill(3)(1).foreach(println)
    //    println(identity("hello"))
    //    println(identity(2.0))

    //    println(safeStringOps("hello",(s:String)=> s.reverse))
    //    def f(s: String) = "f(jhkjh" + s + ")"
    //
    //    def g(s: String) = "g(" + s + ")"
    //
    //    val fComposeG = f _ compose g _
    //    println(fComposeG("hello"))

  }

}
