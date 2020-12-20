package exe_03

import scala.io.Source

// TODO check other scala solution for exe1 and 2

object Exe_03 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_03/test_data.txt"
    val data_path = "src/main/scala/exe_03/data.txt"
    val right = 3
    val down = 1

    def load_data(path: String): (Vector[String], Int, Int) = {
      val data = Source.fromFile(path).getLines.toVector
      val rows = data.size
      val cols = data(0).length
      (data, rows, cols)
    }

    def move(right: Int, down: Int, current_row: Int, current_col: Int, rows: Int, cols: Int): (Int, Int) = {
      ((right + current_col) % cols, if (down + current_row < rows) down + current_row else -1)
    }

    def count_trees_on_path(data: Vector[String], rows: Int, cols: Int, right: Int, down: Int): Int = {

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
    assert(count_trees_on_path(test_data, test_rows, test_cols, right, down) == 7)

    val (data, rows, cols) = load_data(data_path)
    println(count_trees_on_path(data, rows, cols, right, down))
  }
}
