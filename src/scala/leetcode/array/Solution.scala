package leetcode.array

object Solution {

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 1, 2)
    //    arr.filter(_!=1).foreach(println)
    //    removeDuplicates(arr)
    val word1 = Array("ab", "c");
    val word2 = Array("a", "bc")
    //    println(arrayStringsAreEqual(word1,word2))
//    buddyStrings("ab", "ba")
//    val op = splitSent("I love to code in Scala!")
//   println(op)
    transform
  }


  def removeDuplicates(nums: Array[Int]): Int = {

    def removeDuplicatesElems(nums: List[Int]): List[Int] = {

      nums match {
        case Nil => nums
        case head :: tail => head :: removeDuplicatesElems(tail.filter(_ != head))
      }
    }

    val op = removeDuplicatesElems(nums.toList)
    println(op)
    op.length
  }

  def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean = {

    word1.mkString("").equals(word2.mkString(""))
  }

  def buddyStrings(A: String, B: String): Boolean = {

   if(A.length!=B.length){
     return false
   }
    val diff = A.indices.filter(i => A(i) != B(i))
    diff.map(A(_))==diff.map(B(_)).reverse

  }

  def splitSent(sen:String):List[String] = {

    val regex = "\\W"
    val temp = sen.split(" ").map(word=> word.replaceAll(regex,""))
    List(temp(0),temp(1),temp(temp.length-1))
  }

  def transform():Unit = {
    val regex = "\\d"
    val data=Array(Array("Row-Key-001", "K1", "10", "A2", "20", "K3", "30", "B4", "42", "K5", "19", "C20", "20"),
      Array("Row-Key-002", "X1", "20", "Y6", "10", "Z15", "35", "X16", "42"),
      Array("Row-Key-003", "L4", "30", "M10", "5", "N12", "38", "O14", "41", "P13", "8"))

    val temp = data.map(arr => arr.filter(ele => !ele.substring(0,1).matches(regex)).toList).toList


  }
}
