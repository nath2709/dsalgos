package leetcode.Tree

import leetcode.Tree.Medium.TreeNode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashSet, ListBuffer, Queue}


class NodeEX(var _value: Int) {
  var value: Int = _value
  var children: List[NodeEX] = List()
}

object TreeEasy {

  def mergeTrees(t1: TreeNode, t2: TreeNode): TreeNode = {

    if (t1 == null && t2 == null) {
      return null
    }
    else if (t1 == null) return t2
    else if (t2 == null) return t1

    val temp = new TreeNode(t1.value + t2.value)
    temp.left = mergeTrees(t1.left, t2.left)
    temp.right = mergeTrees(t1.right, t2.right)
    println(temp.value)
    temp

  }

  def postorder(root: NodeEX): List[Int] = {

    if (root == null) {
      return List()
    }

    root.children.flatMap(postorder) :+ root.value

  }

  def preorder(root: NodeEX): List[Int] = {

    if (root == null) {
      return List()
    }

    root.value +: root.children.flatMap(preorder)

  }

  def maxDepth(root: NodeEX): Int = {

    if (root == null) {
      return 0
    }
    if (root.children.isEmpty) {
      return 1
    }

    1 + root.children.map(maxDepth).max
  }

  def maxDepth(root: TreeNode): Int = {

    if (root == null) {
      return 0
    }
    if (root != null && root.right == null && root.left == null) {
      return 1
    }
    1 + maxDepth(root.left) + maxDepth(root.right)
  }

  def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean = {

    val lst1 = getLeaves(root1)
    //    println(lst1.mkString(","))
    val lst2 = getLeaves(root2)
    //    println(lst2.mkString(","))
    lst1.equals(lst2)
    false
  }

  def getLeaves(root: TreeNode): List[Int] = {

    root == null match {
      case true => null
      case false => {
        if (root.left == null && root.right == null) return List(root.value)
        else {
          getLeaves(root.left) ++ getLeaves(root.right)
        }
      }

    }


    //      val queue:mutable.Stack[TreeNode] = mutable.Stack()
    //      val lst:ArrayBuffer[Int] = ArrayBuffer()
    //      queue.push(root)
    //      while(!queue.isEmpty ){
    //        val node = queue.pop()
    //        val lft = node.left
    //        val rt = node.right
    //        if(node.left==null && node.right==null){
    //          lst.+=(node.value)
    //        }
    //        if(lft!=null){
    //          queue.push(lft)
    //        }
    //
    //        if(rt!=null){
    //          queue.push(rt)
    //        }
    //      }
    //      lst
  }

  def averageofLevels(root: TreeNode): Unit = {

    if (root == null) {
      return null
    }

    def averageOfLevels(root: TreeNode, buffer: ArrayBuffer[Double]): Array[Double] = {
      if (root == null) {
        return buffer.toArray
      }

      if (root.left != null && root.right != null) {
        buffer.+=((root.left.value + root.right.value) / 2)
      }
      averageOfLevels(root.left, buffer) ++ averageOfLevels(root.right, buffer)

    }

    averageOfLevels(root, ArrayBuffer(root.value))
  }

  def findTarget(root: TreeNode, k: Int): Boolean = {

    def findTarget(root: TreeNode, k: Int, set: HashSet[Int]): Boolean = {
      root match {
        case null => false
        case r => {
          if (set.contains(k - r.value)) {
            return true

          }
          else {
            set += root.value
            findTarget(root.left, k, set) || findTarget(root.right, k, set) || false
          }
        }
      }
    }

    findTarget(root, k, mutable.HashSet())
  }


  def searchBST(root: TreeNode, `val`: Int): TreeNode = {
    if (root == null) {
      return null
    }

    if (root.value >= `val`) {

      if (root.value == `val`) {
        return root
      }
      searchBST(root.left, `val`)
    } else {

      if (root.value == `val`) {
        return root
      }
      searchBST(root.right, `val`)
    }

  }

  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {

    val l = 0
    val r = numbers.length - 1

    @tailrec
    def twoSum(numbers: Array[Int], target: Int, l: Int, r: Int): Array[Int] = {

      if (l > r) {
        return Array()
      }
      val sum = numbers(l) + numbers(r)
      if (sum == target) {
        return Array(l + 1, r + 1)

      } else {
        if (sum < target) {
          twoSum(numbers, target, l + 1, r)
        } else {
          twoSum(numbers, target, l, r - 1)
        }
      }
    }

    twoSum(numbers, target, l, r)
    //       var l = 0
    //       var r = numbers.length-1
    //       var temp:ArrayBuffer[Int] = mutable.ArrayBuffer()
    //
    //       while(l<r){
    //         var sum = numbers(l)+numbers(r)
    //
    //         if(sum==target){
    //           temp+=(l)
    //           temp+=(r)
    //           return temp.toArray
    //         }
    //         if(sum >= target){
    //           r = r-1
    //         }
    //         if(sum< target){
    //           l = l+1
    //         }
    //       }
    //       return temp.toArray
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {

    if (p == null || q == null) {
      return false
    }
    if (p == null && q == null) {
      return true
    }
    if (p.value != q.value) {
      return false
    }
    isSameTree(p.left, q.left) && isSameTree(p.right, q.right)

  }

  //  def minDiffInBST(root: TreeNode): Int = {
  //
  //    if(root==null){
  //      return null
  //    }
  //    root.left
  //  }

  def inorderTraversal(root: TreeNode): List[Int] = {

    if (root == null) {
      return Nil
    }
    inorderTraversal(root.left) ::: List(root.value) ::: inorderTraversal(root.right)


  }

  def minDiffInBST(root: TreeNode): Int = {
    var prev: TreeNode = null
    var min: Int = Int.MaxValue

    def inorder(root: TreeNode): Unit = {

      if (root == null) {
        return
      }
      inorder(root.left)
      if (prev != null) {
        min = Math.min(min, root.value - prev.value)
      }
      prev = root
      inorder(root.right)
    }

    inorder(root.left)
    min

  }

  def bstToGst(root: TreeNode): TreeNode = {
    def test(root: TreeNode, sum: Int): Int = {
      if (root == null) {
        return sum
      }

      root.value = root.value + test(root.right, sum)
      test(root.left, root.value)
    }

    test(root, 0)
    root
  }

  def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] = {

    def getElements(root: TreeNode): List[Any] = {

      if (root == null) return List[Int]()
      getElements(root.left) ::: root.value :: getElements(root.right)

    }

    def mergeLst(lst1: List[Int], lst2: List[Int]): List[Int] = (lst1, lst2) match {
      case (Nil, Nil) => Nil
      case (x :: xs, Nil) => lst1
      case (Nil, y :: ys) => lst2
      case (xs :: x, ys :: y) => {
        if (xs < ys) {
          xs :: mergeLst(lst1.tail, lst2)
        } else {
          ys :: mergeLst(lst1, lst2.tail)
        }
      }
    }

    val lst1: List[Int] = getElements(root1).map(_.toString.toInt)
    val lst2: List[Int] = getElements(root2).map(_.toString.toInt)

    mergeLst(lst1, lst2)
  }

  def insertIntoBST(root: TreeNode, `val`: Int): TreeNode = {

    if (root == null) {
      return new TreeNode(`val`)
    }
    if (`val` > root.value) {
      root.right = insertIntoBST(root.right, `val`)
    } else {
      root.left = insertIntoBST(root.left, `val`)
    }
    root
  }

  //  1161. Maximum Level Sum of a Binary Tree
  def removeLeafNodes(root: TreeNode, target: Int): TreeNode = {

    def remove(root: TreeNode, target: Int): TreeNode = {
      if (root == null) {
        return root
      }

      root.left = remove(root.left, target)
      root.right = remove(root.right, target)

      if (root != null && root.left == null && root.right == null && root.value == target) {
        return null
      }

      root
    }

    remove(root, target)
  }

  def printTree(root: TreeNode): Unit = {

    val queue: Queue[TreeNode] = mutable.Queue()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      println(node.value)
      if (node.left != null) {
        queue.enqueue(node.left)
      }
      if (node.right != null) {
        queue.enqueue(node.right)
      }
    }
    //    if (root == null) {
    //      return
    //    }
    //    println(root.value)
    //    printTree(root.left)
    //
    //    printTree(root.right)
  }

  //  1161. Maximum Level Sum of a Binary Tree
  def maxLevelSum(root: TreeNode): Int = {

    val queue: Queue[(TreeNode, Int)] = collection.mutable.Queue()
    val levelMap: collection.mutable.Map[Int, Int] = collection.mutable.Map()

    if (root != null) {
      queue.enqueue((root, 1))

    }
    //    var level = 2
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      val level = node._2
      if (levelMap.contains(level)) {
        levelMap.put(node._2, levelMap(node._2) + node._1.value)
      } else {
        levelMap.put(node._2, node._1.value)
      }
      if (node._1.left != null) {
        queue.enqueue((node._1.left, level + 1))

      }
      if (node._1.right != null) {
        queue.enqueue((node._1.right, level + 1))
      }
      //      level = level + 1
    }

    var max = Integer.MIN_VALUE
    var min = Integer.MAX_VALUE

    for ((k, v) <- levelMap) {
      val temp = levelMap(k)
      if (max < temp) {
        max = temp
      }
    }

    for ((k, v) <- levelMap) {
      val temp = levelMap(k)
      if (temp == max) {
        if (min > k) {
          min = k
        }
      }

    }
    min

  }

  def maxLevelSum_1(root: TreeNode): Int = {

    val x = LazyList
      .iterate(Seq(root))(level => {
        level.flatMap(node => Seq(Option(node.left), Option(node.right)).flatten)
      }).takeWhile(_.nonEmpty).map(_.map(_.value))

    println(x.mkString(","))
    0
  }

  //  var prev: TreeNode = null
  var max: Int = Integer.MIN_VALUE
  var cnt = 1

  def goodNodes(root: TreeNode): Int = {
    def gn(root: TreeNode, max: Int): Int = {
      if (root == null) {
        return 0
      }
      if (max <= root.value) {

        1 + gn(root.left, root.value) + gn(root.right, root.value)
      } else {
        gn(root.left, max) + gn(root.right, max)
      }
    }

    gn(root, root.value)
  }

  //  def pseudoPalindromicPaths (root: TreeNode): Int = {
  //
  //    def ppath(root:TreeNode,List):List[Any] = {
  //      if(root==null) return Nil
  //
  //      root.value::ppath(root.left):::ppath(root.right)
  //    }
  //
  //    val op = ppath(root)
  //    println(op.mkString(","))
  //    0
  //  }

  def binaryTreePaths(root: TreeNode): List[String] = {

    def paths(root: TreeNode, temp: List[Int], lst: ListBuffer[String]): List[String] = {

      if (root == null) {
        return List()
      }
      paths(root.left, temp.:+(root.value), lst)

      paths(root.right, temp.:+(root.value), lst)

      if (root.left == null && root.right == null) lst += temp.:+(root.value).mkString("->")
      lst.toList
    }

    paths(root, List(), ListBuffer())

  }

  //  841. Keys and Rooms
  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val set: mutable.HashSet[Int] = mutable.HashSet.empty

    def canVisitRoom(room: Int): Unit = {
      if (!set.contains(room)) {
        set += room
        rooms(room).foreach(canVisitRoom)
      }
    }

    canVisitRoom(0)
    set.size == rooms.size
  }

  //  515. Find Largest Value in Each Tree Row
  def largestValues(root: TreeNode): List[Int] = {

    //      val op = LazyList.iterate(Seq(root))(lvl => {
    //        lvl.flatMap(level=> Seq(Option(level.left),Option(level.right)).flatten)
    //      }).takeWhile(_.nonEmpty).map(x=> x.map(_.value).max)

    def largestValues(root: TreeNode, lst: mutable.HashMap[Int, List[Int]], level: Int): mutable.HashMap[Int, List[Int]] = {

      if (root == null) {
        return lst
      }

      if (lst.contains(level)) {
        lst.put(level, lst(level) :+ root.value)
      } else {
        lst.put(level, List(root.value))
      }
      largestValues(root.left, lst, level + 1)
      largestValues(root.right, lst, level + 1)
    }

    val temp: mutable.HashMap[Int, List[Int]] = mutable.HashMap()
    val op = largestValues(root, temp, 0)
    op.values.map(_.max).toList

  }

  //  230. Kth Smallest Element in a BST
  def kthSmallest(root: TreeNode, k: Int): Int = {

    def kthSmallest(root: TreeNode): List[Any] = {

      if (root == null) {
        return List.empty
      }
      kthSmallest(root.left) ::: List(root.value) ::: kthSmallest(root.right)
    }

    val op = kthSmallest(root).map(_.toString.toInt)
    op(k - 1)
  }

  def minReorder(n: Int, connections: Array[Array[Int]]): Int = {
    val op = connections.count(x => {
      //       println(x(0),x(1))
      x(0) < x(1)
    })
    op

  }

  // 863. All Nodes Distance K in Binary Tree
  def distanceK(root: TreeNode, target: TreeNode, K: Int): List[Int] = {

    def distanceTemp(root: TreeNode, target: TreeNode, K: Int, nodeType: String): (Int, String) = {
      //      println(K)
      if (root == null) {
        return (0, "R")
      }
      if (root.value == target.value && nodeType == "L") {
        return (K, "L")
      }
      if (root.value == target.value && nodeType == "R") {
        return (K, "R")
      }
      val lfind = distanceTemp(root.left, target, K + 1, "L")
      val rfind = distanceTemp(root.right, target, K + 1, "R")
      if (lfind._1 != 0) {
        lfind
      } else {
        rfind
      }

    }

    def nodeDistance(root: TreeNode, target: TreeNode, dis: Int, temp: Int, buffer: ListBuffer[Int]): List[Int] = {

      if (root == null) {
        return buffer.toList
      }
      if (dis == temp) {
        buffer += root.value
      }
      nodeDistance(root.left, target, dis, temp + 1, buffer)
      nodeDistance(root.right, target, dis, temp + 1, buffer)
    }

    val op = distanceTemp(root, target, 0, "R")
    println(op)
    //    val temp = nodeDistance(root,target,op+K,0,ListBuffer())
    //    ++ nodeDistance(root,target,K-op,0,ListBuffer())

    //    println(temp.mkString(","))
    Nil
  }

  def treeDepth(root: TreeNode): Int = {
    if (root == null) return 0

    val ldepth = 1 + treeDepth(root.left)
    val rdepth = 1 + treeDepth(root.right)
    return Math.max(ldepth, rdepth)


  }

  def preorderTraversal(root: TreeNode): List[Int] = {

    if (root == null) {
      return List()
    }
    List(root.value) ++ preorderTraversal(root.left) ++ preorderTraversal(root.right)
  }

  def postorderTraversal(root: TreeNode): List[Int] = {

    if (root == null) {
      return List()
    }
    postorderTraversal(root.left) ++ postorderTraversal(root.right) ++ List(root.value)
  }

  def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {

    def findDepth(root: TreeNode, node: Int): Int = {
      if (root == null) {
        return 0
      }
      if (root.value == node) {
        return 0
      } else {
        return 1 + findDepth(root.left, node) + findDepth(root.right, node)
      }

    }

    println(findDepth(root, x))
    return false
  }

  def findParentatDepth(root:TreeNode,depth:Int):TreeNode = {

    def findLeftSide(root: TreeNode, depth: Int): TreeNode = {

      if (root == null) {
        return null
      }
      if (depth == 1) {
//        println(depth, root.value)
        return root
      }
      findLeftSide(root.left,depth-1)
    }

    def findRightSide(root: TreeNode, depth: Int): TreeNode = {

      if (root == null) {
        return null
      }
      if (depth == 0) {
//        println(depth, root.value)
        return root
      }
      findRightSide(root.right,depth-1)
    }

    var p = findLeftSide(root,depth)
    if(p==null){
      p = findRightSide(root,depth)
    }

    p


  }


  def main(args: Array[String]): Unit = {
    val lst1 = List(1, 3, 5, 7, 9)
    val lst2 = List(2, 4, 6, 8, 10, 12, 14, 16)
    val input = Array(Array(0, 1), Array(1, 3), Array(2, 3), Array(4, 0), Array(4, 5))
    //    println(minReorder(6, input))
    //    canVisitAllRooms(input)
    val t1 = new TreeNode(1,
      new TreeNode(2, new TreeNode(4)), new TreeNode(3))

    val op = findParentatDepth(t1, 2)
    println(op.value)
    //    , new TreeNode(7), new TreeNode(4)))
    //      , new TreeNode(1, new TreeNode(0), new TreeNode(8)))

    //    val op = postorderTraversal(t1)
    //    println(op.mkString(","))
    //    distanceK(t1,new TreeNode(5),2)
    //    val op = treeDepth(t1)
    //    println(op)
    //    println(kthSmallest(t1,1))
    //    largestValues(t1)
    //    val op = binaryTreePaths(t1)
    //        println(op)
    //    printTree(t1)
    //    val op = goodNodes(t1)
    //    println(op)
    //    maxLevelSum_1(t1)
    //      new TreeNode(3)
    //      , new TreeNode(2))
    //    val t2 = new TreeNode(4, new TreeNode(1, new TreeNode(0), new TreeNode(2, null, new TreeNode(3))),
    //      new TreeNode(6, new TreeNode(5), new TreeNode(7, null, new TreeNode(8))))

    //    val op = removeLeafNodes(t1, 2)
    //    //    val op = insertIntoBST(t1, 25)
    //    printTree(op)


    //    val op = getAllElements(t2, t1)
    //    println(op)
    //    //    println(test(t2.left,0))
    //    val min = minDiffInBST(t2)
    //    println(min)
    //    inorderTraversal(t2)
    //    val nd = new NodeEX(1)
    //    val nd1 = new NodeEX(3)
    //    nd1.children = List(new NodeEX(5), new NodeEX(6))
    //    nd.children = List(nd1, new NodeEX(2), new NodeEX(4))

    //    val ex = new NodeEX(1)
    //    val ex1 = new NodeEX(3)
    //
    //    val ex_ch = List(ex1, new NodeEX(2), new NodeEX(4))
    //    val ex_ch_1 = List(new NodeEX(5), new NodeEX(6))
    //    ex.children = ex_ch
    //    ex1.children = ex_ch_1

    //    println(maxDepth(ex))

    //    val min_ex = new TreeNode(2, new TreeNode(1), new TreeNode(3, new TreeNode(7), new TreeNode(4, new TreeNode(8), new TreeNode(5, new TreeNode(10), new TreeNode(6)))))
    //    println(maxDepth(min_ex))
    //    val op1 = getLeaves(min_ex)
    //    println(op1)

    //    val root1 = new TreeNode(3, new TreeNode(5, new TreeNode(6), new TreeNode(2, new TreeNode(7), new TreeNode(4))),
    //      new TreeNode(1, new TreeNode(9), new TreeNode(8)))
    //    val root2 = new TreeNode(3, new TreeNode(5, new TreeNode(6), new TreeNode(7)),
    //      new TreeNode(1, new TreeNode(4), new TreeNode(2, new TreeNode(9), new TreeNode(8))))

    //    val op = leafSimilar(root1,root2)
    //    println(op)
    //    mergeTrees(t1,t2)

    //    val op = searchBST(t2, 2)
    //    val op = preorder(nd)
    //    op.foreach(println)
  }
}
