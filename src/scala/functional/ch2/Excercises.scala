package functional.ch2

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Excercises {


  def main(args: Array[String]): Unit = {
    //    println(fib(3))
    println(LongestWord("I love dogs"))
  }

  def LongestWord(sen: String): String = {

    //   split sentences into words
    val words = sen.split(" ")
    //   remove puntuations and clean words
    val cleanWords = words.map(word => word.replaceAll(
      "[^a-zA-Z0-9]", "")).map(word => (word.length, word))
    //    find word with largest length
    val longest_length = cleanWords.sortBy(_._1).last._1
    //    filter words with largest length and return first word from it
    val res = cleanWords.filter(tup => tup._1 == longest_length).head._2
    return res;
  }


}

