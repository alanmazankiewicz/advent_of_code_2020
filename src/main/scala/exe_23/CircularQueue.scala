package exe_23

class Node(var ele: Int, var next: Option[Node], var head: CircularList){

  def add(ele: Int): Node = {
    var new_node = new Node(ele, next, this.head)
    next = Option(new_node)
    new_node
  }

  def merge(node: Node): Node = { // todo use circularity
    // tail of this gets lost
    this.next = Option(node)
    node.head = this.head
    node
  }

  def split(): (CircularList, CircularList) = { // TODO not done
    // Assumes you dont split at the end
    var head_node_after_split = this.next.getOrElse(this)
    this.next = None

    // could lead to bug if not carefull use TODO use circularity -> get head
    var sec_head = new CircularList(head_node_after_split.ele)
    head_node_after_split.getOrElse(this).head = sec_head

  }
}

class CircularList (ele: Int){
  var headnode = new Node(ele, None, this)
  // headnode.next = headnode.next
}

object Main {
  def main(args: Array[String]): Unit = {

    var test = new CircularList(1)
    var test_head = test.headnode
    var tmp = test_head.add(2)
    tmp.add(3)

    var test2 = new CircularList(10)
    var test_head2 = test2.headnode
    var last_test = test_head2.add(20)
    last_test.add(30)

    tmp.merge(last_test)

    println("a")
  }
}