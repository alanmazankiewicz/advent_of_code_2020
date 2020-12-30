package exe_19

import scala.io.Source

object Exe_19 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_19/"
    val rules = Source.fromFile(path + "rules.txt").getLines.toList
    val data = Source.fromFile(path + "data.txt").getLines.toList

    def parse_rule(parsed_rules: Map[String, List[List[String]]], rule: String): Map[String, List[List[String]]] = {
      val splitted = rule.split(": ")
      if (splitted(1) == "a" || splitted(1) == "b") parsed_rules.updated(splitted(0), List(List(splitted(1).trim())))
      else {
        val disjunctions = splitted(1).split("\\|") map { x => x.trim() }
        val sub_automata = (disjunctions map { str => str.split(" ").toList }).toList
        parsed_rules.updated(splitted(0), sub_automata)
      }
    }

    def parse_all_rules(raw_rules: List[String]): Map[String, List[List[String]]] = {
      val empty_map: Map[String, List[List[String]]] = Map()
      raw_rules.foldLeft(empty_map)(parse_rule)
    }

    def parse_data(data: List[String]): Set[String] = {
      val empty_set: Set[String] = Set()
      data.foldLeft(empty_set)({ (set, x) => set + x })
    }

    def count_valid_rules(rules_map: Map[String, List[List[String]]], data: Set[String]): Int = {

      def combine_lists(fst: List[String], sec: List[String]): List[String] = { // TODO more efficient because of string concat linear time
        for {
          a <- fst
          b <- sec
        } yield a + b
      }

      def process_element(ele: String): List[String] = {
        if (ele == "a" || ele == "b") List(ele)
        else process_rules(rules_map(ele))
      }

      def process_rule(rule: List[String]): List[String] = {
        rule map process_element reduce combine_lists
      }

      def process_rules(rules: List[List[String]]):List[String] = {
        rules flatMap process_rule
      }

      val all_rules = process_rules(rules_map("0"))
      all_rules count { x => data.contains(x) }
    }

    println(count_valid_rules(parse_all_rules(rules), parse_data(data)))

    // End
  }
}
