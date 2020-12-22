package exe_07

import scala.io.Source


object Exe_07 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_07/test_data.txt"
    val full_test_data = Source.fromFile(test_data_path).getLines.toList

    val data_path = "src/main/scala/exe_07/data.txt"
    val full_data = Source.fromFile(data_path).getLines.toList

    def get_source_bags(with_val: Boolean)(splitted: Array[String], start_pos: Int, offset: Int): List[String] = {

      def loop(result: List[String], current_pos: Int): List[String] = {
        val bag =  splitted(current_pos) + splitted(current_pos+1)
        val final_bag = if (with_val) splitted(current_pos -1) + bag else bag
        val new_result = final_bag :: result
        val new_pos = current_pos + offset
        if (new_pos > splitted.length) new_result
        else loop(new_result, new_pos)
      }
      loop(Nil, start_pos)
    }

    def from_source_to_target(source_bags: List[String], target_bag: String): List[(String, String)] = {
      for (source_bag <- source_bags) yield (source_bag, target_bag)
    }

    def from_target_to_source(source_bags: List[String], target_bag: String): List[(String, String)] = {
      for (source_bag <- source_bags) yield (target_bag, source_bag)
    }

    def parse_line(direction: (List[String], String) => List[(String, String)], source_bag_func: (Array[String], Int, Int) => List[String]) (line: String): List[(String, String)] = {
      val splitted: Array[String] = line.split(" ")
      val target_bag: String = splitted(0) + splitted(1)
      val source_bags = source_bag_func(splitted, 5, 4)

      direction(source_bags, target_bag)
    }

    def parse_rules(parse_line: String => List[(String, String)])(rules: List[String]): Map[String, List[String]] = {
      rules flatMap parse_line groupBy(_._1) map {x => (x._1, x._2 map { y => y._2 }) }
    }

    def get_upstream_bag_colors(bag_color: String, rules: Map[String, List[String]]): Set[String] = {

      def loop(bag_color: String, current_set: Set[String] = Set()): Set[String] = {

        def catch_helper(current_set: Set[String], bag_color: String): Set[String] = {
          if (current_set contains bag_color) current_set
          else loop(bag_color, current_set + bag_color)
        }

        val upstream_bag_option: Option[List[String]] = rules.get(bag_color)
        upstream_bag_option match {
          case None => current_set
          case Some(lst) => lst.foldLeft(current_set)(catch_helper)
        }
      }
      loop(bag_color)
    }

    def get_down_stream_bag_counts(bag_color: String, rules: Map[String, List[String]]): Int = {

      def loop(bag_color: String, current_count: Int, catch_map: Map[String, Int]): Int = {

        def catch_helper(accumulator: (Int, Map[String, Int]), count_bag_color: String): (Int, Map[String, Int]) = {
          val (current_total_count, catch_map) = accumulator
          val bag_count: Int = if (count_bag_color == "nootherbag.") 0 else count_bag_color(0).asDigit
          val bag_color = count_bag_color.substring(1)

          val check_catch: Option[Int] = catch_map.get(bag_color)
          check_catch match {
            case Some(new_count) => (current_total_count + new_count * bag_count, catch_map)
            case None => {
              val new_count = loop(bag_color, 1, catch_map)  // TODO: return catch map
              val new_map = catch_map + (bag_color -> new_count)
              (current_total_count + bag_count * new_count, new_map)
            }
          }
        }
        if (bag_color == "ootherbags.") 0
        else {
          val downstream_bags: List[String] = rules(bag_color)
          downstream_bags.foldLeft((current_count, catch_map))(catch_helper)._1
        }
      }

      loop(bag_color, 0, Map())
    }


    val source_to_target_parse: List[String] => Map[String, List[String]] = parse_rules(parse_line(from_source_to_target, get_source_bags(false)))

    val test = get_upstream_bag_colors("shinygold", source_to_target_parse(full_test_data)) // 4
    println(test.size)
    val result = get_upstream_bag_colors("shinygold", source_to_target_parse(full_data)) // 370
    println(result.size)

    // Part 2

    val target_to_source_parse: List[String] => Map[String, List[String]] = parse_rules(parse_line(from_target_to_source, get_source_bags(true)))
    val test_2 = target_to_source_parse(full_test_data)
    val test_result = get_down_stream_bag_counts("shinygold", test_2)
    println(test_result)

    println("JSJS")
    // End
  }
}
