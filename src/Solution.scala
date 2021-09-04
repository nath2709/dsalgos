

// * Definition for singly-linked list.
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution {
  def reverseList(head: ListNode): ListNode = {

    
    "".toLowerCase()
    var current: ListNode = head
    var prev: ListNode = null
    var next: ListNode = null
    while (current != null) {

      next = current.next
      current.next = prev
      prev = current
      current = next
      //      temp1.next = temp

    }

    prev
  }
  def main(args: Array[String]): Unit = {

    var x = new ListNode(1)
    val x1 = new ListNode(2)
    val x2 = new ListNode(3)
    x.next = x1
    x1.next = x2

    x = reverseList(x)
    while (x != null) {

      println(x.x)
      x = x.next
    }
  }

}