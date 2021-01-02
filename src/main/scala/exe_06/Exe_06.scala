package exe_06

import scala.io.Source


object Exe_06 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_06/test_data.txt"
    val full_test_data = Source.fromFile(test_data_path).getLines.toList

    val data_path = "src/main/scala/exe_06/test_data.txt"
    val full_data = Source.fromFile(data_path).getLines.toList

    def parse_form(line: String): Set[Char] = {
      line.toSet
    }

    def parse_group(forms: List[String]): (Set[Char], List[String]) = {

      def loop_lines(current_set: Set[Char], forms: List[String]): (Set[Char], List[String]) = {
        forms match {
          case Nil => (current_set, forms)
          case head :: tail if head == "" => (current_set, tail)
          case head :: tail => loop_lines(current_set ++ parse_form(head), tail)
        }
      }

      loop_lines(Set(), forms)
    }

    def parse_group_intersec(forms: List[String]): (Set[Char], List[String]) = {

      def loop_lines(current_list: List[Set[Char]], forms: List[String]): (List[Set[Char]], List[String]) = {
        forms match {
          case Nil => (current_list, forms)
          case head :: tail if head == "" => (current_list, tail)
          case head :: tail => loop_lines(parse_form(head) :: current_list, tail)
        }
      }

      val loop_result = loop_lines(Nil, forms)
      val current_group = loop_result._1
      val union = current_group reduceLeft { (x, y) => x.intersect(y)}
      (union, loop_result._2)
    }

    def parse_all_forms(forms: List[String], parse_group: List[String] => (Set[Char], List[String])): List[Set[Char]] = {

      def loop(forms: List[String], result: List[Set[Char]]): List[Set[Char]] = {
        val (current_set, rest) = parse_group(forms)
        if (rest == Nil) current_set :: result
        else loop(rest, current_set :: result)
      }

      loop(forms, Nil)
    }

    val parsed_test_forms_1 = parse_all_forms(full_test_data, parse_group)
    val test_result_1 = (parsed_test_forms_1 map { x => x.size.toLong }).sum
    println(test_result_1)

    val parsed_forms_1 = parse_all_forms(full_data, parse_group)
    val result_1 = (parsed_forms_1 map { x => x.size }).sum
    println(result_1)

    val parsed_test_forms_2 = parse_all_forms(full_test_data, parse_group_intersec)
    val test_result_2 = (parsed_test_forms_2 map { x => x.size.toLong }).sum
    println(test_result_2)

    val parsed_forms_2 = parse_all_forms(full_data, parse_group_intersec)
    val result_2 = (parsed_forms_2 map { x => x.size }).sum
    println(result_2)

    println("Ks")
    // End
  }
}
