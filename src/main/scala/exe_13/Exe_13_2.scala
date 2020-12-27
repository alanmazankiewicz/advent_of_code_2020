package exe_13

import scala.io.Source

object Exe_13_2 {

  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_13/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_13/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def parse_input(input: Vector[String]): List[(Long, Long)] = {
      val splitted = input(1).split(",")
      val zipped = splitted.zipWithIndex
      (zipped filter { x => x._1 != "x" } map { x => (x._1.toLong, x._2.toLong) }).toList
    }

    def run(input: Vector[String]): Long = {
      val busses = parse_input(input)

      def check(current_timestamp: Long, busses: List[(Long, Long)], step_size: Long): (List[(Long, Long)], Long) = {
        if ((busses == Nil) || ((current_timestamp + busses.head._2) % busses.head._1 != 0))
          (busses, step_size)
        else  check(current_timestamp, busses.tail, step_size * busses.head._1) // The last arg is the key to solving that exe efficiently

      }

      def loop(current_timestamp: Long, busses: List[(Long, Long)], step_size: Long): Long = {
        val (new_busses, new_step_size) = check(current_timestamp, busses, step_size)
        if (new_busses == Nil) current_timestamp
        else loop(current_timestamp + new_step_size, new_busses, new_step_size)
      }
      loop(1, busses, 1)
    }


    println(run(test_data))
    println(run(data))

  }
}
