package exe_10

import scala.io.Source
import scala.math.abs

object Exe_10 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_10/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toList map (_.toInt)

    val data_path = "src/main/scala/exe_10/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toList map (_.toInt)

    def part_1(data: List[Int]): Int = {
      val sorted_data = 0 :: data.sorted
      val differences = (sorted_data sliding (2)).toList map { x => abs(x.head - x.tail.head) }
      val count_ones = differences count (_ == 1)
      val count_threes = differences count (_ == 3)
      count_ones * (count_threes + 1)
    }

    def part_2(data: List[Int]): Long = {
      val finish = data.max + 3
      val data_set = (finish :: data).toSet
      val catch_map: Map[Int, Long] = Map()

      def catch_helper(accum: (Long, Map[Int, Long]), curent_val: Int): (Long, Map[Int, Long]) = {
        val (accum_count, catch_map) = accum
        catch_map.get(curent_val) match {
          case Some(count) => (count + accum_count, catch_map)
          case None => {
            val (count, new_map) = loop(curent_val, catch_map)
            (count + accum_count, new_map + (curent_val -> count))
          }
        }
      }

      def loop(current_val: Int, catch_map: Map[Int, Long]): (Long, Map[Int, Long]) = {
        if (current_val == finish) (1, catch_map)
        else {
          val choices = List(current_val + 1, current_val + 2, current_val + 3)
          val valid_choices = choices filter { x => data_set contains x }
          (valid_choices foldLeft (0:Long, catch_map))(catch_helper)
        }
      }
      loop(0, catch_map)._1
    }

    println(part_1(test_data))
    println(part_1(data))
    println(part_2(test_data))
    println(part_2(data))

    // End
  }
}
