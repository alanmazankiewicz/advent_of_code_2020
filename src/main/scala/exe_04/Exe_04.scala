package exe_04

import scala.io.Source


object Exe_04 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_04/test_data_2.txt"
    val data_path = "src/main/scala/exe_04/test_data.txt"

    val full_test_data = Source.fromFile(test_data_path).getLines.toList
    val full_data = Source.fromFile(data_path).getLines.toList

    val required_fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")

    val byr_pattern = "^([1][9][2-9][0-9]|[2][0][0][0-2])$"
    val iyr_pattern = "^([2][0][1][0-9]|2020)$"
    val eyr_pattern = "^([2][0][2][0-9]|2030)$"
    val hgt_pattern = "(^([1][5-8][0-9]|[1][9][0-3])cm$)|(^([6][0-9]|[7][0-6]|59)in$)"
    val hcl_pattern = "^#[a-f,0-9]{6}$"
    val ecl_pattern = "^(amb|blu|brn|gry|grn|hzl|oth)$"
    val pid_pattern = "^[0-9]{9}$"
    val cid_pattern = ".*"

    val pattern_map = Map(("byr", byr_pattern), ("iyr", iyr_pattern), ("eyr", eyr_pattern),
      ("hgt", hgt_pattern), ("hcl", hcl_pattern), ("ecl", ecl_pattern), ("pid", pid_pattern),
      ("cid", cid_pattern))

    def parse_line(line: String): Map[String, String] = {
      val splitted = line.split(" ")
      (splitted map { x => x.split(":") } map (arr => arr(0) -> arr(1))).toMap
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

    def validate_all_passport_keys(required_fields: Set[String], parsed_passports: List[Map[String, String]]): List[Boolean] = {
      parsed_passports map { x => (x.keySet + "cid") == required_fields }
    }

    def filter_valid_passports(parsed_passports: List[Map[String, String]], valid_keys: List[Boolean]): List[Map[String, String]] = {
      (parsed_passports zip valid_keys) filter { x => x._2 } map { x => x._1 }
    }

    def validate_valid_passport_fields(validation_map: Map[String, String], valid_passports: List[Map[String, String]]): List[Boolean] = {

      def check_passport(passport: Map[String, String]): Boolean = {
        (passport map { case (key, value) => value.matches(validation_map(key)) }) forall {x => x}
      }

      valid_passports map check_passport
    }

    val parsed_test_passports = parse_all_passports(full_test_data)
    val valid_test_pass_keys = validate_all_passport_keys(required_fields, parsed_test_passports)
    val valid_test_passports = validate_valid_passport_fields(pattern_map, filter_valid_passports(parsed_test_passports, valid_test_pass_keys))

    val test_result_keys =  valid_test_pass_keys count { x => x }
    val test_result_key_fields = valid_test_passports count { x => x }
    println(test_result_keys)
    println(test_result_key_fields)
    println("")

    val parsed_passports = parse_all_passports(full_data)
    val valid_pass_keys = validate_all_passport_keys(required_fields, parsed_passports)
    val valid_passports = validate_valid_passport_fields(pattern_map, filter_valid_passports(parsed_passports, valid_pass_keys))

    val result_pt_1 =  valid_pass_keys count { x => x }
    val result_pt_2 = valid_passports count { x => x }
    println(result_pt_1)
    println(result_pt_2)


    // End
  }
}
