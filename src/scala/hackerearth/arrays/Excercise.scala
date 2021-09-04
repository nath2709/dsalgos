package hackerearth.arrays

object Excercise {

  def binQueries(): Unit = {

  }

  def canyousolfit(): Unit = {
    val input = scala.io.StdIn.readLine()
    val testCases = input.toInt

    for (i <- 0 until testCases) {
      val N = scala.io.StdIn.readLine().toInt
      val array = new Array[Int](N)
      val input = scala.io.StdIn.readLine().split("\\s")
      for (i <- 0 until input.length) {
        //        println(input(i))
        array(i) = input(i).toInt

        

      }
      
      var max = Integer.MIN_VALUE
        for (i <- 0 until array.length) {
          for (j <- i until array.length) {
            val temp = Math.abs(array(i) - array(j)) + Math.abs(i - j)
            if (temp > max) {
              max = temp
            }
          }
        }
      println("max =" + max)
      
      //      array.foreach(println)
    }

  }

  def main(args: Array[String]): Unit = {
    //    val name = scala.io.StdIn.readLine() // Reading input from STDIN
    //    println("Hi, " + name + ".")
    //    val x = Array(3)
    //    x.foreach(println)
    canyousolfit
  }
}