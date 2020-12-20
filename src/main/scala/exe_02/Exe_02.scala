package exe_02

import scala.io.Source

// TODO check other scala solution for exe1 and 2

object Exe_02 {
  def main(args: Array[String]): Unit = {

    val data_path = "src/main/scala/exe_02/data.txt"
    val data = Source.fromFile(data_path).getLines.toList
    val test_case_1 = "1-3 a: abcde" // true
    val test_case_2 = "2-3 c: ccccccccc" // false
    val test_case_3 = "2-20 c: ccaaaaaaaa" // true
    val test_case_4 = "2-3 c: aaaac" // false
    val test_case_5 = "2-3 c: aaaacc" // true
    val test_data_1 = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc") // 2
    val test_data_2 = List(test_case_1, test_case_2, test_case_3, test_case_4, test_case_5) // 3

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
        case List(f1, f2, f3, f4) => (f1, f2, f3, f4)
      }
    }

    def check_password(input_tuple: (String, String, String, String)): Boolean = {
      val (min, max, target_char, password) = input_tuple
      val password_length = password.length

      def loop(lower: Int, upper: Int, target_char: Char, password_list: List[Char], password_length: Int): Boolean = {
        password_list match {
          case Nil => (lower <= 0) && (upper >= 0)
          case head :: tail if upper < 0 => false
          case head :: tail if lower > password_length => false
          case head :: tail if (upper > password_length) && (lower <= 0) => true
          case head :: tail if head == target_char => loop(lower - 1, upper - 1, target_char, password_list.tail, password_length - 1)
          case _ => loop(lower, upper, target_char, password_list.tail, password_length - 1)
        }
      }

      loop(min.toInt, max.toInt, target_char(0), password.toList, password_length)
    }

    def get_correct_pw_count(data: List[String]): Int = {
      data map parse_string map check_password count { x => x }
    }

    def test_all(test_data: List[String], correct_pws: List[Boolean], correct_count: Int): Unit = {
      val checked_data = test_data map parse_string map check_password
      assert(checked_data == correct_pws, checked_data)
      assert(get_correct_pw_count(test_data) == correct_count, get_correct_pw_count(test_data))
    }

    test_all(test_data_1, List(true, false, true), 2)
    test_all(test_data_2, List(true, false, true, false, true), 3)

    println(get_correct_pw_count(data))

  }
}
