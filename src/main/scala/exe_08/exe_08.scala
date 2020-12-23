package exe_08

import scala.io.Source


object exe_08 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_08/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_08/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def parse_line(line: String): (String, Int) = {
      val splitted = line.split(" ")
      (splitted(0), splitted(1).toInt)
    }

    def parse_data(data: Vector[String]): Vector[(String, Int)] = {
      data map parse_line
    }

    def run_instructions(instructions: Vector[(String, Int)]): Int = {

      def loop(acc: Int, idx: Int, past_instructions: Set[Int]): Int = {
        if (past_instructions contains idx) acc
        else {
          val (insturct, value) = instructions(idx)
          val (new_acc, new_idx) = insturct match {
            case "nop" => (acc, idx + 1)
            case "acc" => (acc + value, idx + 1)
            case "jmp" => (acc, idx + value)
          }
          loop(new_acc, new_idx, past_instructions + idx)
        }
      }
      loop(0, 0, Set())
    }


    def debug_instructions(instructions: Vector[(String, Int)]): Int = {
      // using catch would be more efficient -> no time for that

      def loop(acc: Int, idx: Int, past_instructions: Set[Int], switchable: Boolean): Option[Int] = {
        if (past_instructions contains idx) None
        else if (idx >= instructions.size) Some(acc)
        else {
          val (insturct, value) = instructions(idx)
          insturct match {
            case "acc" => loop(acc + value, idx + 1, past_instructions + idx, switchable)
            case "jmp" => {
              val left = loop(acc, idx + value, past_instructions + idx, switchable)
              if (left == None && switchable) loop(acc, idx + 1, past_instructions + idx, false) else left
            }
            case "nop" => {
              val left = loop(acc, idx + 1, past_instructions + idx, switchable)
              if (left == None && switchable) loop(acc, idx + value, past_instructions + idx, false) else left
            }
          }
        }
      }
      loop(0, 0, Set(), true).getOrElse(0)
    }

    println(run_instructions(parse_data(test_data)))
    println(run_instructions(parse_data(data)))
    println(debug_instructions(parse_data(test_data)))
    println(debug_instructions(parse_data(data)))

    // END
  }
}
