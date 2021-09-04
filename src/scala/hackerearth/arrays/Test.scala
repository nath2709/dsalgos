package hackerearth.arrays

// Sample code to perform I/O:
object Test extends App {

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

  }
}
