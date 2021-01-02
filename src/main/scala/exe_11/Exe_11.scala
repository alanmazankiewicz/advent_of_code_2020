package exe_11
import scala.io.Source


object Exe_11 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_11/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_11/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def adjacent_idxes(row: Int, col: Int, conditions: (Int, Int) => Boolean): List[(Int, Int)] = {
      (for {
        height_delta <- (-1 to 1)
        width_delta <- (-1 to 1)
        new_row = row + height_delta
        new_col = col + width_delta
        if conditions(new_row, new_col)
      } yield (new_row, new_col)).toList
    }

    def directional_idxes(grid: Vector[String])(row: Int, col: Int, conditions: (Int, Int) => Boolean): List[(Int, Int)] = {
      // I could do the count already here (which would be slightly more efficient),
      // but this way this function fits nicely into the code of part 1.

      val movements = (for {
        height_delta <- (-1 to 1)
        width_delta <- (-1 to 1)
        if (height_delta, width_delta) != (0,0)
      } yield (height_delta, width_delta)).toList

      def search_direction(row_col_delta: (Int, Int)): (Int, Int) = {
        val (row_delta, col_delta) = row_col_delta

        def loop(row: Int, col: Int): (Int, Int) = {
          val new_row = row + row_delta
          val new_col = col + col_delta
          if (conditions(new_row, new_col)) {
            if (grid(new_row)(new_col) != '.') (new_row, new_col)
            else loop(new_row, new_col)
          }
          else (-1, -1)
        }
        loop(row, col)
      }
      movements map search_direction filter { x => x._1 != -1 }
    }

    def surrounding(neighboors: (Int, Int, (Int, Int) => Boolean) => List[(Int, Int)])(pos: (Int, Int))(implicit grid: Vector[String]): List[(Int, Int)] = {
      val height = grid.size
      val width = grid(0).size
      val (row, col) = pos

      def conditions (new_row: Int, new_col: Int): Boolean = {
        (new_row >= 0) && (new_row < height) && (new_col >= 0) && (new_col < width) && (pos != (new_row, new_col))
      }
      neighboors(row, col, conditions)
    }

    def get_occupied_seat_count(pos_surround: List[(Int, Int)])(implicit grid: Vector[String]): Int = {
      pos_surround count { x => grid(x._1)(x._2) == '#' }
    }

    def get_new_seat_state(pos: (Int, Int))(occupied_seat_count: Int, min_seat_count: Int)(implicit grid: Vector[String]): Char = {
      val current_seat_state = grid(pos._1)(pos._2)
      current_seat_state match {
        case 'L' => if(occupied_seat_count == 0) '#' else 'L'
        case '#' => if(occupied_seat_count > min_seat_count) 'L'  else '#'
        case '.' => '.'
      }
    }

    def create_all_pos(implicit grid: Vector[String]): IndexedSeq[IndexedSeq[(Int, Int)]] = {
      val height = grid.size
      val width = grid(0).size

      for (row <- 0 until height) yield for (col <- 0 until width) yield (row, col)
    }

    def iteration(grid: Vector[String])(implicit neighboors: (Int, Int, (Int, Int) => Boolean) => List[(Int, Int)], min_seat_count: Int): Vector[String] = {
      implicit val impl_grid:Vector[String] = grid
      (for (inner_seq <- create_all_pos) yield (for(pos <- inner_seq) yield get_new_seat_state(pos)(get_occupied_seat_count(surrounding(neighboors)(pos)), min_seat_count)).mkString).toVector
    }

    def check_convergence(previous_grid: Vector[String], new_grid: Vector[String]): Boolean = {
      previous_grid zip new_grid forall {x => x._1 == x._2}
    }

    def count_seats(implicit grid: Vector[String]): Int = {
      (grid map {line => line count { c => c == '#'} }).sum
    }

    def run(grid: Vector[String])(implicit neighboors: (Int, Int, (Int, Int) => Boolean) => List[(Int, Int)], min_seat_count: Int): Int = {
      val next_grid = iteration(grid)
      if (check_convergence(grid, next_grid)) count_seats(next_grid)
      else run(next_grid)
    }

    println(run(test_data)(adjacent_idxes, 3))
    println(run(data)(adjacent_idxes, 3))

    // Part2
    println(run(test_data)(directional_idxes(test_data), 4))
    println(run(data)(directional_idxes(data), 4))

    // END
  }
}
