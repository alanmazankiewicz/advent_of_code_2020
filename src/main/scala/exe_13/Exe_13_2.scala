package exe_13

import scala.io.Source

object Exe_13_2 {

  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_13/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_13/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def test(value: Long):Unit = { // TODO Remove
      if(value < 0) println(value)
    }

    def parse_input(input: Vector[String]): List[(Long, Long)] = {
      val splitted = input(1).split(",")
      val zipped = splitted.zipWithIndex
      (zipped filter { x => x._1 != "x" } map { x => (x._1.toLong, x._2.toLong) }).toList
    }

    def next_divisor_of_k_greater_x(x: Long)(k: Long): Long = {
      val remainder = (x + k) % k
      x + k - remainder
    }

    def run(input: Vector[String]): Long = {
      val parsed_input = parse_input(input)
      val base_val = parsed_input.head._1
      val busses = parsed_input.tail

      def next_timestamp(last_timestamp: Long): Long = last_timestamp + base_val

      def check(current_timestamp: Long, busses: List[(Long, Long)], current_result: Boolean): Boolean = {
        if (!current_result || busses == Nil) current_result
        else {
          val (bus_id, offset) = busses.head
          val normalized_timestamp = next_divisor_of_k_greater_x(current_timestamp)(bus_id) - offset
          check(current_timestamp, busses.tail, current_timestamp == normalized_timestamp)
        }
      }

      def loop(current_timestamp: Long): Long = {
        test(current_timestamp)
        if(check(current_timestamp, busses, true)) current_timestamp
        else loop(next_timestamp(current_timestamp))
      }
      loop(0)
    }


    println(run(test_data))
    println(run(data))

  }
}
