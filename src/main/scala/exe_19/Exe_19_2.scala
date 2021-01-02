package exe_19

import scala.io.Source
import scala.collection.mutable

object Exe_19_2 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_19/"
    val rules = Source.fromFile(path + "rules.txt").getLines.toList
    val data = Source.fromFile(path + "test_data.txt").getLines.toList

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

    def count_valid_rules(rules_map: Map[String, List[List[String]]], data: Set[String]): (Int, Int) = {

      def get_regexes(from: Int): (String, Map[String, String]) = {

        val catch_mem: mutable.Map[String, String] = mutable.Map()

        def combine_rule(builder: StringBuilder, str: String): StringBuilder = {
          builder ++= str
        }

        def combine_rules(fst: String, sec: String): String = {
          "(" + fst + "|" + sec + ")"
        }

        def process_element(ele: String): String = {
          if (ele == "a" || ele == "b") ele
          else {
            catch_mem.get(ele) match {
              case Some(substring) => substring
              case None => {
                val processed = process_rules(rules_map(ele))
                catch_mem += (ele -> processed)
                processed
              }
            }
          }
        }

        def process_rule(rule: List[String]): String = {
          (rule map process_element).foldLeft(new StringBuilder)(combine_rule).mkString
        }

        def process_rules(rules: List[List[String]]): String = {
          rules map process_rule reduce combine_rules // only works if there is max one "|" per rule
        }

        ("^" + process_rules(rules_map(from.toString)) + "$", catch_mem.toMap)
      }

      val (main_regex, regex_map) = get_regexes(0)

      val snd_regex: String = {

        val four = regex_map("42")
        val three = regex_map("31")

        def combine_rules(buffer: StringBuilder, str: String): StringBuilder = {
          buffer ++= str
          buffer ++= "|"
        }

        val body = (1 to 10) map { x => s"($four){$x}($three){$x}" } reduce { (x,y) => x + "|" + y }

        s"^($four)+(" + body + ")$"
      }

      (data count { x => x.matches(main_regex) }, data count { x => x.matches(snd_regex) })
    }

    println(count_valid_rules(parse_all_rules(rules), parse_data(data)))


    // End
  }
}
