package exe_07

import scala.io.Source


object Exe_07 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_07/test_data.txt"
    val full_test_data = Source.fromFile(test_data_path).getLines.toList

//    val data_path = "src/main/scala/exe_07/data.txt"
//    val full_data = Source.fromFile(data_path).getLines.toList

    def get_source_bags(splitted: Array[String], start_pos: Int, offset: Int): List[String] = {

      def loop(result: List[String], current_pos: Int): List[String] = {
        val new_result = (splitted(current_pos) + splitted(current_pos+1)) :: result
        val new_pos = current_pos + offset
        if (new_pos > splitted.length) new_result
        else loop(new_result, new_pos)
      }
      loop(Nil, start_pos)
    }

    def parse_line(line: String): List[(String, String)] = {
      val splitted: Array[String] = line.split(" ")
      val target_bag: String = splitted(0) + splitted(1)
      val source_bags = get_source_bags(splitted, 5, 4)

      for (source_bag <- source_bags) yield (source_bag, target_bag)
    }

    def parse_rules(rules: List[String]): Map[String, List[String]] = {
      rules flatMap parse_line groupBy(_._1) map {x => (x._1, x._2 map { y => y._2 }) }
    }

    def get_upstream_bag_colors(bag_color: String, rules: Map[String, List[String]]): Set[String] = {

      def loop(bag_color: String, current_set: Set[String] = Set()): Set[String] = {
        val upstream_bag_option: Option[List[String]] = rules.get(bag_color)
        upstream_bag_option match {
          case None => current_set
          case Some(lst) => lst map {x => loop(x, current_set + x)} reduceLeft { (x,y) => x union y }
        }
      }
      loop(bag_color)
    }



    val test = get_upstream_bag_colors("shinygold", parse_rules(full_test_data))
    println("asdas")




    // End
  }
}
