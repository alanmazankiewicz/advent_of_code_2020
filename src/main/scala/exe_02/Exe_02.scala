package exe_02

import scala.io.Source

// TODO check other scala solution for exe1 and 2

object Exe_02 {
  def main(args: Array[String]): Unit = {

    // val data_path = "src/main/scala/exe_02/data.txt"
    // val raw_data = Source.fromFile(data_path).getLines.toList
    // val data = raw_data.map(_.toInt)
    val test_case = "1-3 a: abcde"
    val test_data = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")

    def parse_string(password_policy: String): (String, String, String, String) = {
      val split_rules = List(": ", " ", "-")

      def loop(rest: String, split_rules: List[String], result: List[String]): List[String] = {
        if (split_rules == Nil) rest :: result
        else {
          val split_rule = split_rules.head
          val splitted_string = rest.split(split_rule)
          loop(splitted_string(0), split_rules.tail, splitted_string(1) :: result)
        }
      }
      loop(password_policy, split_rules, Nil) match {
        case List(f1,f2,f3,f4) => (f1,f2,f3,f4)
      }
    }

    def check_password(input_tuple: (String, String, String, String)): Boolean = {
      val (min, max, target_char, password) = input_tuple

      def loop(lower: Int, upper: Int, target_char: Char, password_list: List[Char]): Boolean = {
        password_list match {
          case Nil => (lower <= 0) && (upper >= 0)
          case head :: tail if head == target_char => loop(lower-1, upper-1, target_char, password_list.tail)
          case _ => loop(lower, upper, target_char, password_list.tail)
        }
      }
      loop(min.toInt, max.toInt, target_char(0), password.toList)
    }

    val result = test_data map parse_string map check_password count {x => x}

    println(parse_string(test_case))
    println(check_password(parse_string(test_case)))
    println(result)
  }
}
