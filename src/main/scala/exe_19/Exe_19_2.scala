package exe_19

import scala.io.Source

object Exe_19_2 {

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

      def combine_rule(fst: String, sec: String): String = { // TODO stringbuffer and foldLeft, catch
        fst + sec
      }

      def combine_rules(fst: String, sec: String): String = {
        "(" + fst + "|" + sec + ")"
      }

      def process_element(ele: String): String = {
        if (ele == "a" || ele == "b") ele
        else process_rules(rules_map(ele))
      }

      def process_rule(rule: List[String]): String = {
        rule map process_element reduce combine_rule
      }

      def process_rules(rules: List[List[String]]): String = {
        rules map process_rule reduce combine_rules
      }

      def get_regex(from: Int): String = "^" + process_rules(rules_map(from.toString)) + "$"

      val main_regex = get_regex(0)

      data count {x => x.matches(main_regex)}
    }

    println(count_valid_rules(parse_all_rules(rules), parse_data(data)))




    ///
    //      def find_startsWith(data_point: String, prefixes: List[String]): Boolean = {
    //        prefixes find { x => data_point startsWith(x) } match {
    //          case Some(i) => true
    //          case None => false
    //        }
    //      }
    //
    //      def check(data_point_check: String => Boolean, prefixes: List[String]): Boolean = {
    //        prefixes find { x => data_point_check(x) } match {
    //          case Some(i) => true
    //          case None => false
    //        }
    //      }
    //
    //      val test = Set("bbabbbbaabaabba", "babbbbaabbbbbabbbbbbaabaaabaaa", "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    //      "bbbbbbbaaaabbbbaaabbabaaa", "bbbababbbbaaaaaaaabbababaaababaabab", "ababaaaaaabaaab", "ababaaaaabbbaba", "baabbaaaabbaaaababbaababb",
    //      "abbbbabbbbaaaababbbbbbaaaababb", "aaaaabbaabaaaaababaa", "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")
    //
    //      val fourty = process_rules(rules_map("42"))
    //      val thirty = process_rules(rules_map("31"))
    //
    //      val reg_ex_rules = {
    //        for {
    //          four_1 <- fourty.toStream
    //          four_2 <- fourty.toStream
    //          four_3 <- fourty.toStream
    //          four_4 <- fourty.toStream
    //          four_5 <- fourty.toStream
    //          three_1 <- thirty.toStream
    //          three_2 <- thirty.toStream
    //          three_3 <- thirty.toStream
    //          three_4 <- thirty.toStream
    //        } yield (s"^($four_1)+(($four_2){1}($three_1){1}|($four_3){2}($three_2){2}|($four_4){3}($three_3){3}|($four_5){4}($three_4){4})" + "$")
    //      }
    //
    //      val tester = reg_ex_rules filter { x => x.startsWith("^(bbabb)+((bbaab){1}")}
    //
    //      def check_2(expression: String): Boolean = {
    //        reg_ex_rules exists { regex => expression.matches(regex) }
    //      }
    //
    //      val counted = data count check_2
    //
    //      val eight_rules = process_rules(rules_map("8"))
    //      val prefixes = eight_rules ::: combine_lists(eight_rules, eight_rules) ::: (List(eight_rules, eight_rules, eight_rules) reduce combine_lists) ::: (List(eight_rules, eight_rules, eight_rules, eight_rules) reduce combine_lists)
    //
    //      val eleven_rules = process_rules(rules_map("11"))
    //      val th_rules = process_rules(rules_map("31"))
    //      // val suffixes = eleven_rules ::: combine_lists(eleven_rules, th_rules)
    //      // val suffixes = th_rules
    //      val suffixes = (List(th_rules, th_rules, th_rules) reduce combine_lists) ::: combine_lists(th_rules, th_rules) ::: th_rules ::: (List(th_rules, th_rules, th_rules, th_rules) reduce combine_lists)
    //
    //      // val count = data count { x => check(x.startsWith, prefixes) && check(x.endsWith, suffixes) }
    //
    //
    //
    //
    ////      //  other approch
    ////
    ////      val fourty = process_rules(rules_map("42"))
    ////
    ////      val fst = List(fourty, fourty, th_rules) reduce combine_lists
    ////      val sec = List(fourty,  fourty, fourty, th_rules, th_rules) reduce combine_lists
    ////      val third = List(fourty, fourty, fourty, fourty, th_rules, th_rules, th_rules) reduce combine_lists
    ////      // val last = List(fourty, fourty, fourty, fourty, fourty, th_rules, th_rules, th_rules, th_rules) reduce combine_lists
    ////
    ////      val complete = (fst ::: sec ::: third).toSet
    ////
    ////      val count_2 = data count { x => complete.contains(x) }

    // End
  }
}
