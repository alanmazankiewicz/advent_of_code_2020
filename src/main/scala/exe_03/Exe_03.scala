package exe_03

import scala.io.Source


object Exe_03 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_03/test_data.txt"
    val data_path = "src/main/scala/exe_03/test_data.txt"
    val right = 3
    val down = 1
    val all_moves = List((1,1), (3,1), (5,1), (7,1), (1,2))

    def load_data(path: String): (Vector[String], Int, Int) = {
      val data = Source.fromFile(path).getLines.toVector
      val rows = data.size
      val cols = data(0).length
      (data, rows, cols)
    }

    def move(right: Int, down: Int, current_row: Int, current_col: Int, rows: Int, cols: Int): (Int, Int) = {
      ((right + current_col) % cols, if (down + current_row < rows) down + current_row else -1)
    }

    def count_trees_on_path(data: Vector[String], rows: Int, cols: Int)(right: Int, down: Int): Int = {

      def loop(current_row: Int, current_col: Int, current_count: Int): Int = {
        val (new_col, new_row) = move(right, down, current_row, current_col, rows, cols)
        if (new_row == -1) current_count
        else {
          val square_type = data(new_row)(new_col)
          val new_count = if (square_type == '#') current_count + 1 else current_count
          loop(new_row, new_col, new_count)
        }
      }

      loop(0, 0, 0)
    }

    val (test_data, test_rows, test_cols) = load_data(test_data_path)
    val test_count_trees: (Int, Int) => Int = count_trees_on_path(test_data, test_rows, test_cols)
    assert(test_count_trees(right, down) == 7)

    val (data, rows, cols) = load_data(data_path)
    val apply_count_trees: (Int, Int) => Int = count_trees_on_path(data, rows, cols)
    println(apply_count_trees(right, down)) // 234
    assert(apply_count_trees(right, down) == 234)

    val test_part_2 = (all_moves map {x => test_count_trees(x._1, x._2)}).product
    assert(test_part_2 == 336)

    println((all_moves map {x => apply_count_trees(x._1, x._2).toLong}).product)
  }
}
