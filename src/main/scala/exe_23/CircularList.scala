package exe_23

import scala.annotation.tailrec

class Node(var ele: Int){

  var next: Node = this

  def add_last(ele: Int): Node = {
    val new_node = new Node(ele)
    new_node.next = next
    next = new_node
    new_node
  }

  def create_from_lst(data: List[Int]): Unit = {
    ele = data.head
    data.tail.foldLeft(this)((acc, value) => acc.add_last(value))
  }

  def merge(node: Node): Unit = {
    // tail of this gets lost
    val tmp_next = next
    this.next = node
    node.next.next.next = tmp_next
  }

  def isIn(value: Int): Boolean = {
    (value == ele) || (value == next.ele) || (value == next.next.ele)
  }

  @tailrec
  final def find(value: Int): Node = {
    if(ele == value) this
    else next.find(value)
  }


  def split(): Node = {
    // Assumes you dont split at the end
    val head_node_after_split = next
    val last_after_split = head_node_after_split.next.next

    this.next = last_after_split.next

    head_node_after_split
  }
}


