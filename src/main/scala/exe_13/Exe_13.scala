package exe_13

import scala.io.Source

object Exe_13 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_13/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_13/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def parse_input(input: Vector[String]): (Int, Vector[Int]) = {
      val timestamp = input(0).toInt
      val busses = (input(1).split(",") filter { x => x != "x" } map { value => value.toInt }).toVector
      (timestamp, busses)
    }

    def next_divisor_of_k_greater_equal_x(x: Int)(k: Int): Int = {
      val remainder = (x + k) % k
      if (remainder == 0) x else x + k - remainder
    }

    def find_next_bus(parsed_input: (Int, Vector[Int])): (Int, Int) = {
      val (timestamp, busses) = parsed_input
      val next_dep = busses map next_divisor_of_k_greater_equal_x(timestamp)
      val next_bus_timestamp_idx = next_dep.zipWithIndex.min
      val next_bus = (busses(next_bus_timestamp_idx._2), next_bus_timestamp_idx._1)
      next_bus
    }

    def calc_result(timestamp: Int, next_bus: (Int, Int)): Int = {
      (next_bus._2 - timestamp) * next_bus._1
    }

    def run(input: Vector[String]): Int = {
      val parsed_input = parse_input(input)
      val next_bus = find_next_bus(parsed_input)
      calc_result(parsed_input._1, next_bus)
    }

    println(run(test_data))
    println(run(data))



    println(next_divisor_of_k_greater_equal_x(1068781)(13))

  }
}
