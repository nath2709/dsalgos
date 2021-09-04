package leetcode.Tree

import scala.collection.mutable.{ArrayBuffer, Queue, Stack}

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

//Definition for a Node.
class Node(var _value: Int) {
  var value: Int = _value
  var prev: Node = null
  var next: Node = null
  var child: Node = null
}

object Medium {

  def reverseParentheses(s: String): String = {

    val temp: Stack[Int] = Stack()
    var str = s
    for (i <- 0 until str.length()) {
      val bracket = str.charAt(i)
      if (bracket == '(') {
        temp.push(i)
      }
      if (bracket == ')') {

        val idx = temp.pop
        println(idx, i)
        str = str.substring(0, idx) + str.substring(idx + 1, i).reverse + str.substring(i + 1, str.length())

      }
    }
    str

    //    val
    //    val map: Map[Char, List[Int]] = Map()
    //    val x = s.toCharArray().zipWithIndex.filter(p => p._1 == '(' || p._1 == ')').foldLeft(map) {
    //      case (op, x) =>
    //        val list = op.getOrElse(x._1, List[Int]())
    //        op + (x._1 -> (list :+ x._2))
    //    }
    //    val (lst_1, lst_2) = x.splitAt(1)
    //
    //    println(lst_1)
    //    println(lst_2)

    //    println(x._1.map(_._1).mkString(","))
    //    println(x._2.map(_._2).mkString(","))

    //    var i = indexes.length / 2 - 1;
    //    var j = i + 1
    //    var idx = 0
    //    var temp = s
    //    while (idx < indexes.length / 2) {
    //      temp = temp.substring(0, indexes(i)) + temp.substring(indexes(i), indexes(j)).reverse + temp.substring(indexes(j), temp.length())
    //      i = i - 1;
    //      j = j + 1;
    //      idx = idx + 1
  }

  def subsets(nums: List[Int]): List[List[Int]] = {

    for (i <- 0 until nums.length) {

      println(nums(i))
      subsets(nums.drop(i))

    }
    //    def subsets(nums: List[Int], res: List[List[Int]]): List[List[Int]] = nums match {
    //
    //      case h::Nil       => List(h)::res
    //      case h :: tail => subsets(tail, List(h) :: res)
    //      case _         => Nil
    //
    //    }
    //    subsets(nums, List(List()))
    Nil
  }

  def getIndexes(s: String, index: Int): List[Int] = {

    if (s.isEmpty()) {
      List(0)
    }
    val idx = s.indexOf('(')

    val (head, tail) = s.splitAt(idx)
    println(s, idx, head)
    //    getIndexes(tail, idx)
    List(0)

  }

  def nextLargerNodes(head: ListNode): Array[Int] = {

    val arr: ArrayBuffer[Int] = ArrayBuffer()

    var currNode = head
    while (currNode != null) {
      val currVal = currNode.x
      var nextNode = currNode.next

      while (nextNode != null) {
        val temp = nextNode.x
        println(temp, currVal)
        if (currVal < temp) {
          arr.+=(temp)
        }
        nextNode = nextNode.next
      }
      currNode = currNode.next

    }
    println(arr.mkString(","))
    arr.toArray
  }

  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {

    var stack: List[Int] = List()
    val op = List()
    stack = stack :+ 0
    while (!stack.isEmpty) {

      val item = graph(stack.head)
      //      if (stack.head == graph.length - 1) {
      //
      //        println(stack)
      //      }
      stack = stack.tail
      for (ele <- 0 until item.length) {
        stack = stack :+ item(ele)
      }
    }

    Nil
  }

  def dfs(arr: Array[Array[Int]], row: Int, column: Int) {

    if (row >= 0 && row <= arr.length - 1 && column >= 0 && column <= arr(0).length - 1 && arr(row)(column) == 1) {
      arr(row)(column) = 0
      dfs(arr, row - 1, column)
      dfs(arr, row + 1, column)
      dfs(arr, row, column - 1)
      dfs(arr, row, column + 1)
    }
  }

  def numEnclaves(A: Array[Array[Int]]): Int = {

    for (i <- 0 until A.length) {
      for (j <- 0 until A(0).length) {
        //         println(i,j)
        if (i == 0 || i == A.length - 1 || j == 0 || j == A(i).length - 1) {
          //          println(i,j)
          dfs(A, i, j)
        }

      }
    }
    var count = 0
    for (i <- 0 until A.length) {
      for (j <- 0 until A(i).length) {
        if (A(i)(j) == 1) {
          count = count + 1
        }
      }
    }

    count

  }

  def flatten(head: Node): Node = {

    if (head == null) {
      return head
    }

    print(head._value + ",")
    val ch = head.child
    val nxt = head.next
    if (ch != null) {
      head.next = ch
      ch.prev = head
      flatten(ch)
    }
    if (nxt != null) {
      head.next = nxt
      nxt.prev = head
      flatten(nxt)
    }
    head

  }

  def distanceK(root: TreeNode, target: TreeNode, K: Int): List[Int] = {

    val depth = findDepth(root, target.value)
    println(depth._1, depth._2)
    //    var queue: collection.mutable.Queue[TreeNode] = Queue()
    //    var lst: ArrayBuffer[Int] = ArrayBuffer()
    //    queue.enqueue(target)
    //    var count = 0
    //    while (!queue.isEmpty) {
    //
    //      val ele = queue.dequeue()
    //      val left = ele.left
    //      val right = ele.right
    //      count = count + 1
    //      if (count / 2 == K) {
    //        lst += ele.value
    //      }
    //      if (left != null) {
    //        queue.enqueue(ele.left)
    //      }
    //
    //      if (right != null) {
    //        queue.enqueue(ele.right)
    //      }
    //
    //    }
    //    println(lst.mkString(","))
    //    lst.toList
    null
  }

  def findDepth(root: TreeNode, target: Int): (Int, Boolean) = {

    var queue: collection.mutable.Queue[TreeNode] = Queue()

    queue.enqueue(root)
    var count = 0
    var isleft = true

    while (!queue.isEmpty) {

      val ele = queue.dequeue()
      //      println(ele.value)
      count = count + 1

      if (ele._value == target) {
        return (count / 2, isleft)
      }
      if (ele.left != null) {
        queue.enqueue(ele.left)
        isleft = true
      }

      if (ele.right != null) {
        queue.enqueue(ele.right)
        isleft = false
      }

    }

    (count / 2, isleft)
  }

  def dfs(root: TreeNode): Int = {

    if (root == null) {
      return 0
    }

    val left_depth = 1 + dfs(root.left)
    val right_depth = 1 + dfs(root.right)

    math.max(left_depth, right_depth)

  }

  def deepestLeavesSum(root: TreeNode): Int = {

    def deepestLeavesSum(root: TreeNode, depth: Int): Int = {

      //      println(root.value,depth)
      if (root == null) {
        return 0
      }
      if (depth == 1) {
        return root.value
      }

      val left_val = deepestLeavesSum(root.left, depth - 1)
      val right_val = deepestLeavesSum(root.right, depth - 1)
      left_val + right_val

      //      val right_val = deepestLeavesSum(root.right, depth - 1)

    }

    val depth = dfs(root)
    println("depth", depth)
    deepestLeavesSum(root, depth)
    //    val right_val = deepestLeavesSum(root.right, depth-1)
    //    val right_val = deepestLeavesSum(root,depth)
  }

  var gp: TreeNode = null
  var p: TreeNode = null
  var sum = 0

  def sumEvenGrandparent(root: TreeNode): Int = {

    var sum = 0

    def sumEvenGrandparent(root: TreeNode, parent: TreeNode, gp: TreeNode): Unit = {

      if (root == null) {
        return
      }
      if (gp != null && gp.value % 2 == 0) {
        sum = sum + root.value
      }

      sumEvenGrandparent(root.left, root, parent)
      sumEvenGrandparent(root.right, root, parent)
    }
    sumEvenGrandparent(root,null,null)
    sum
  }


  def main(args: Array[String]): Unit = {
    //"sxmdll(q)eki(x)"
    var s = "sxmdll(q)eki(x)"

    val root = new TreeNode(6,
      new TreeNode(7, new TreeNode(2, new TreeNode(9, null, null), null),
        new TreeNode(7, new TreeNode(1, null, null), new TreeNode(4, null, null))),
      new TreeNode(8, new TreeNode(1, null, null),
        new TreeNode(3, null, new TreeNode(5, null, null))))

    val op = sumEvenGrandparent(root)
    println(op)
    //    println(reverseParentheses(s))
    //    println(subsets(List(1, 2, 3)))

    //    val x = new ListNode(2, new ListNode(1, new ListNode(5, null)))
    //    nextLargerNodes(x)
    //    allPathsSourceTarget(Array(Array(1, 2), Array(3), Array(3), Array()))
    //    val array = Array(Array(0, 0, 0, 1, 1, 1, 0, 1, 0, 0), Array(1, 1, 0, 0, 0, 1, 0, 1, 1, 1), Array(0, 0, 0, 1, 1, 1, 0, 1, 0, 0), Array(0, 1, 1, 0, 0, 0, 1, 0, 1, 0), Array(0, 1, 1, 1, 1, 1, 0, 0, 1, 0), Array(0, 0, 1, 0, 1, 1, 1, 1, 0, 1), Array(0, 1, 1, 0, 0, 0, 1, 1, 1, 1), Array(0, 0, 1, 0, 0, 1, 0, 1, 0, 1), Array(1, 0, 1, 0, 1, 1, 0, 0, 0, 0), Array(0, 0, 0, 0, 1, 1, 0, 0, 0, 1))
    //    println(numEnclaves(array))

    //    val root = new TreeNode(
    //      3,
    //      new TreeNode(
    //        5,
    //        new TreeNode(6, null, null),
    //        new TreeNode(2, new TreeNode(7, null, null),
    //          new TreeNode(4, null, null))), new TreeNode(1, new TreeNode(0, null, null), new TreeNode(8, null, null)))

    //    val res = distanceK(root, new TreeNode(
    //      5, new TreeNode(6, null, null),
    //      new TreeNode(2, new TreeNode(7, null, null),
    //        new TreeNode(4, null, null))), 2)
    //    println(res)
    //    val l1 = new TreeNode(5)
    //    val r1 = new TreeNode(1)
    //    val l2 = new TreeNode(6)
    //    val r2 = new TreeNode(2)
    //    val l3 = new TreeNode(7)
    //    val r3 = new TreeNode(4)

    //    val head = new Node(1)
    //    val two = new Node(2)
    //    val three = new Node(3)
    //    val four = new Node(4)
    //    val five = new Node(5)
    //    val six = new Node(6)
    //    val seven = new Node(7)
    //    val eight = new Node(8)
    //    val nine = new Node(9)
    //    val ten = new Node(10)
    //    val ele = new Node(11)
    //    val twe = new Node(12)
    //
    //    head.next = two
    //    two.next = three
    //    three.next = four
    //    three.child = seven
    //    seven.next = eight
    //    eight.next = nine
    //    eight.child = ele
    //    ele.next = twe
    //    nine.next = ten
    //    four.next = five
    //    five.next = six
    //    six.child = null
    //    six.next = null
    //
    //    //    var op = flatten(head)
    //    //
    //    //    while (op != null) {
    //    //      println(op.value)
    //    //      op = op.next
    //    //    }

    //    val root = new TreeNode(1, new TreeNode(2, new TreeNode(4, new TreeNode(7, null, null), null), new TreeNode(5, null, null)), new TreeNode(3, null, new TreeNode(6, null, new TreeNode(8, null, null))))
    //    val op = deepestLeavesSum(root)
    //    println(op)

  }

  class TreeNode(var _value: Int, var _left: TreeNode=null, var _right: TreeNode=null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

}