package exe_05

import scala.io.Source

// TODO check other scala solution for exe1 and 2 and 3, 4


object Exe_05 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_05/test_data.txt"
    val data_path = "src/main/scala/exe_05/data.txt"

    val full_test_data = Source.fromFile(test_data_path).getLines.toList
    val full_data = Source.fromFile(data_path).getLines.toList

    def search_seat(partial_boarding_pass: String): Int = {
      val init_char = partial_boarding_pass(0)
      val (min_seat, max_seat, move_down) = {
        if (init_char == 'F' || init_char == 'B') (0.0, 127.0, 'F') else (0.0, 7.0, 'L')
      }

      def next_seat_range(current_seat_range: (Double, Double), movement: Char): (Double, Double) = {
        val middle: Double = (current_seat_range._1 + current_seat_range._2) / 2.0
        if (movement == move_down) (current_seat_range._1, middle.floor)
        else (middle.ceil, current_seat_range._2)
      }

      (partial_boarding_pass.foldLeft((min_seat, max_seat))(next_seat_range))._1.toInt
    }

    def get_seat_id(row_col: (Int, Int)): Int = row_col._1 * 8 + row_col._2

    def transform_boarding_pass(boarding_pass: String): (Int, Int) = {
      val splitted = boarding_pass.splitAt(7)
      (search_seat(splitted._1), search_seat(splitted._2))
    }

    def increment_seat(seat: (Int, Int)): (Int, Int) = {
      val new_col = seat._2 + 1
      if (new_col > 7) (seat._1 + 1, 0)
      else (seat._1, new_col)
    }

    def search_empty_seat(seats: List[(Int, Int)]): (Int, Int) = {
      val sorted_seats = seats.sorted

      def search(sorted_seats: List[(Int,Int)], target_seat: (Int, Int)): (Int, Int) = {
        val head_seat = sorted_seats.head
        if (head_seat != target_seat) target_seat
        else search(sorted_seats.tail, increment_seat(target_seat))
      }
      search(sorted_seats, sorted_seats.head)
    }


    val test = full_test_data map transform_boarding_pass map get_seat_id

    val seats = full_data map transform_boarding_pass
    val result_1 = (seats map get_seat_id).max
    println(result_1)

    val result_2 = get_seat_id(search_empty_seat(seats))
    println(result_2)

    // End
  }
}
