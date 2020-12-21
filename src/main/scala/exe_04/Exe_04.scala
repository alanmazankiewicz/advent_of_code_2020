package exe_04

import scala.io.Source
import scala.util.matching.Regex

// TODO check other scala solution for exe1 and 2 and 3


object Exe_04 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_04/test_data.txt"
    val data_path = "src/main/scala/exe_04/data.txt"

    val full_test_data = Source.fromFile(test_data_path).getLines.toList
    val full_data = Source.fromFile(data_path).getLines.toList

    val required_fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")

    val byr_pattern = "^([1][9][2-9][0-9]|[2][0][0][0-2])$"
    val iyr_pattern = "^([2][0][1][0-9]|2020)$"
    val eyr_pattern = "^([2][0][2][0-9]|2030)$"
    val hgt_pattern = "(^([1][5-8][0-9]|[1][9][0-3]) cm$)|(^([6][0-9]|[7][0-6]|59) in$)"
    val hcl_pattern = "^#[a-f,0-9]{6}$"



    def parse_line(line: String): Map[String, String] = {
      val splitted = line.split(" ")
      (splitted map { x => x.split(":")} map (arr => arr(0) -> arr(1))).toMap
    }

    def parse_passport(passports: List[String]): (Map[String, String], List[String]) = {

      def loop_passport(current_map: Map[String, String], passports: List[String]): (Map[String, String], List[String]) = {
        passports match {
          case Nil => (current_map, passports)
          case head :: tail if head == "" => (current_map, tail)
          case head :: tail => loop_passport(current_map ++ parse_line(head), tail)
        }
      }

      loop_passport(Map(), passports)
    }

    def parse_all_passports(passports: List[String]): List[Map[String, String]] = {

      def loop(passports: List[String], result: List[Map[String, String]]): List[Map[String, String]] = {
        val (current_maps, rest) = parse_passport(passports)
        if (rest == Nil) current_maps :: result
        else loop(rest, current_maps :: result)
      }

      loop(passports, Nil)
    }

    def validate_all_passports(required_fields: Set[String], parsed_passports: List[Map[String, String]]): List[Boolean] = {
      parsed_passports map { x => (x.keySet + "cid") == required_fields }
    }

    val test_result_pt_1 = validate_all_passports(required_fields, parse_all_passports(full_test_data)) count { x => x }
    assert(test_result_pt_1 == 2)

    val result_pt_1 = validate_all_passports(required_fields, parse_all_passports(full_data)) count { x => x }
    println(result_pt_1)



    println("69 in".matches(hgt_pattern))
    println("")

    // End
  }
}
