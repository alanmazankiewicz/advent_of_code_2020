package exe_21

import scala.collection.mutable
import scala.io.Source

object Exe_21 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_21/data.txt"
    val data = Source.fromFile(path).getLines.mkString

    def parser_1(raw_data: String): (List[(String, Set[String])], List[String]) = {
      val reg = "\\((.*?)\\)"
      val ingred = (raw_data.split(reg) map { x => x.trim().split(" ").toSet }).toList
      val allerg_tmp = (reg.r.findAllIn(raw_data) map { x => x.replace("(contains ", "").replace(")", "") }).toList
      val allerg = allerg_tmp map { x => x.split(", ") }
      val result = (ingred zip allerg) flatMap { x => x._2 map { y => (y, x._1) } }
      (result, ingred.flatten)
    }

    def part_1(raw_data: String): Int = {
      val (data, all_ingred) = parser_1(raw_data)
      val has_allerg = (data.groupBy(x => x._1) map { x => x._2 map { y => y._2 } } flatMap { x => x reduce { (set_1, set_2) => set_1.intersect(set_2) } }).toSet
      val each_ingred = all_ingred.toSet
      val no_allerg = each_ingred.diff(has_allerg)
      all_ingred count { x => no_allerg.contains(x) }
    }

    val result_1 = part_1(data)
    println(result_1)

    def part_2(raw_data: String): String = {
      val (data, all_ingred) = parser_1(raw_data)
      val tmp = data.groupBy(x => x._1) map { x => (x._1, x._2 map { y => y._2 }) }
      val res = (tmp map { x => (x._1, x._2 reduce { (set_1, set_2) => set_1.intersect(set_2) }) }).toMap

      val mut_res = collection.mutable.Map(res.toSeq: _*)
      val final_res: mutable.ArrayBuffer[(String, String)] = new mutable.ArrayBuffer()

      def del_from_res_sets(ingred: String): Unit = {
        for {
          (k, v) <- mut_res
        } mut_res(k) = v - ingred
      }

      while (mut_res.nonEmpty) {
        val filtered_ingred = mut_res filter { x => x._2.size == 1 }
        filtered_ingred map { x =>
          final_res += ((x._1, x._2.head))
          mut_res -= x._1
          del_from_res_sets(x._2.head)
        }
      }

      (final_res.sortBy(_._1) map { x => x._2 }).mkString(",")
    }

    val result_2 = part_2(data)
    println(result_2)
  }
}
