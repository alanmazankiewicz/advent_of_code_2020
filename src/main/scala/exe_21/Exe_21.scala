package exe_21

import scala.io.Source

object Exe_21 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_21/data.txt"
    val data = Source.fromFile(path).getLines.mkString

    def parser_1(raw_data: String): (List[(String, Set[String])], List[String]) = {
      val reg = "\\((.*?)\\)"
      val ingred = (raw_data.split(reg) map { x => x.trim().split(" ").toSet }).toList
      val allerg_tmp = (reg.r.findAllIn(raw_data) map { x => x.replace("(contains ", "").replace(")", "") }).toList
      val allerg = allerg_tmp map { x => x.split(", ")}
      val result = (ingred zip allerg) flatMap { x => x._2 map { y => (y, x._1) } }
      (result, ingred.flatten)
    }

    def part_1(raw_data: String): Int = {
      val (data, all_ingred) = parser_1(raw_data)
      //val x = data.groupBy(x => x._1) map { x => x._2 map { y => y._2 } } map { x => x reduce { (set_1, set_2) => set_1.intersect(set_2) } }  // determine which is which
      val has_allerg = (data.groupBy(x => x._1) map { x => x._2 map { y => y._2 } } flatMap { x => x reduce { (set_1, set_2) => set_1.intersect(set_2) } }).toSet
      val each_ingred = all_ingred.toSet
      val no_allerg = each_ingred.diff(has_allerg)
      all_ingred count { x => no_allerg.contains(x) }
    }

    val result_1 = part_1(data)
    println(result_1)

    def part_2(raw_data: String): String = {
      val (data, all_ingred) = parser_1(raw_data)
      "jaja"
    }

    println("s")
  }
}
