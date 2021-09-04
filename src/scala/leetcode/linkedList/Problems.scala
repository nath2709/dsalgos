package leetcode.linkedList

import leetcode.Tree.ListNode

object Problems {

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {

    (l1, l2) match {
      case (null, l2) => l2
      case (l1, null) => l1
      case (l1, l2) if (l1.x <= l2.x) => {
        l1.next = mergeTwoLists(l1.next, l2)
        l1
      }
      case (l1, l2) if (l1.x > l2.x) => {
        l2.next = mergeTwoLists(l1, l2.next)
        l2
      }
    }
  }

  def deleteDuplicates(head: ListNode): ListNode = {

    var current = head
    while (current != null && current.next != null) {
      if (current.x == current.next.x) {
        current.next = current.next.next
      } else {
        current = current.next
      }
    }
    head
  }

  def isPalindrome(head: ListNode): Boolean = {
    if (head == null) {
      return false
    }
    isPalindrome(head.next)
    println(head.x)
    false
  }

  def reverseList(head: ListNode): ListNode = {

    var current = head
    var prev: ListNode = null
    var next: ListNode = null
    while (current != null) {
      next = current.next
      current.next = prev
      prev = current
      current = next
    }
    prev

  }

  def removeElements(head: ListNode, `val`: Int): ListNode = {
    var temp = head
    var prev: ListNode = new ListNode()
    prev.next = head
    var curr = prev

    while (temp != null) {
      if (head.x != `val`) {
        curr.next = head
        curr = curr.next
      }
      temp = temp.next
    }
    prev.next
  }

  def mergeInBetween(list1: ListNode, a: Int, b: Int, list2: ListNode): ListNode = {

    var i = 1
    var start = list1
    var temp = list2
    while (i < a) {
      start = start.next
      i = i + 1
    }
    //    println(start.x,i)
    var end = start
    //    i = 0
    while (i <= b) {
      //      println(i,b)
      end = end.next
      i = i + 1
    }
    start.next = temp
    while (temp.next != null) {
      temp = temp.next
    }
    temp.next = end.next

    list1

  }

  def swapNodes(head: ListNode, k: Int): ListNode = {

    var counter = 1
    var start = head

    var length = 0
    while (start != null) {
      start = start.next
      length = length + 1
    }
    start = head

    while (counter < k) {
      start = start.next
      counter = counter + 1
    }

    counter = 1
    var idx = length - (k - 1)
    var end = head


    while (counter < idx) {

      end = end.next
      counter = counter + 1
    }
    val temp = start.x
    start.x = end.x
    end.x = temp
    head
  }

  // 328. Odd Even Linked List
  def oddEvenList(head: ListNode): ListNode = {

    var odd = head
    var even = head.next
    var evenHead = even
    while (even != null && even.next != null) {
      odd.next = odd.next.next
      even.next = even.next.next
      odd = odd.next
      even = even.next
    }
    odd.next = evenHead
    head
  }

  def reverse(head: ListNode): ListNode = {

    def reverse(head: ListNode, tail: ListNode): ListNode = {
      if (head == null) {
        return null
      }
      reverse(head.next, tail)
      head
    }

    reverse(head, new ListNode())

  }

  def createList(tmp: List[Int]): ListNode = {

    var lst = tmp
    var head = new ListNode()
    var temp = head
    while (!lst.isEmpty) {
      head.next = new ListNode(lst.head)
      lst = lst.tail
      head = head.next
    }
    temp.next
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var t1 = reverse(l1)
    var t2 = reverse(l2)
    var sum = 0
    var carry = 0
    var head = new ListNode()
    val temp = head

    while (t1 != null && t2 != null) {
      sum = t1.x + t2.x + carry
      carry = sum / 10
      sum = sum % 10
      head.next = new ListNode(sum)
      head = head.next
      t1 = t1.next
      t2 = t2.next
    }

    while (t1 != null) {
      sum = t1.x + carry
      carry = sum / 10
      sum = sum % 10
      head.next = new ListNode(sum)
      head = head.next
      t1 = t1.next
    }
    while (t2 != null) {
      sum = t2.x + carry
      carry = sum / 10
      sum = sum % 10
      head.next = new ListNode(sum)
      head = head.next
      t2 = t2.next
    }
    if (carry != 0) {
      head.next = new ListNode(carry)
      head = head.next
    }
    reverse(temp.next)
  }

  def fib(n:Int):Int = {
    if(n<2){
      return 1
    }
     fib(n-1)+ fib(n-2)
  }

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode(1, new ListNode(2, new ListNode(3)))
    val l2 = new ListNode(5)

    val memo:Array[Int] = Array(0,1,0,0,0,0)
//    println(fib(5,memo))

//    println(5/3,5%3)

    //    var op = mergeTwoLists(l1,l2)
    //    var op = deleteDuplicates(l1)
    //    var op = removeElements(l1,2)
    //    var op = mergeInBetween(l1,3,4,l2)
    //    var op = createList(List(1,2,3,4))
    //    var op = reverse(l1)
    //    println("-------------")
    //    while (op != null) {
    //      print(op.x+"->")
    //      op = op.next
    //    }
  }
}
