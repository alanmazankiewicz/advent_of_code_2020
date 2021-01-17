package exe_23


object Exe_23b {
  def main(args: Array[String]): Unit = {
    val data = "624397158".map(_.asDigit).toList

    def play(inp: List[Int], moves: Int ): Long = {
      val init_lst = new Node(0)
      init_lst.create_from_lst(inp)

      val max_val = inp.max

      def create_node_table(starting_val: Int, node: Node, table: Map[Int, Node]): Map[Int, Node]  = {
        if(node.ele == starting_val) table + (node.ele -> node)
        else create_node_table(starting_val, node.next, table + (node.ele -> node))
      }

      val node_table = create_node_table(init_lst.ele, init_lst.next, Map[Int, Node]())

      def loop(n: Int, current_node: Node): Unit = {
        if (n == 0) return
        else {
          val pick_up = current_node.split()
          var destination_val = current_node.ele

          do{
            destination_val -= 1
            if(destination_val == 0) destination_val = max_val
          } while(pick_up.isIn(destination_val))

          val destination_node = node_table(destination_val)
          destination_node.merge(pick_up)
          loop(n-1, current_node.next)
        }
      }

      loop(moves, init_lst)
      val one_node = node_table(1)
      one_node.next.ele.toLong * one_node.next.next.ele.toLong
    }

    def trans_inp(inp: List[Int]): List[Int] = {
      val max_val = inp.max + 1
      inp ::: (max_val to 1000000).toList
    }

    val result = play(trans_inp(data), 10000000)
    println(result)


    // END
  }
}
