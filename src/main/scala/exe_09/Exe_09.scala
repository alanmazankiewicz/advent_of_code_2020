package exe_09

import scala.io.Source


object Exe_09 {
  def main(args: Array[String]): Unit = {


    val test_data_path = "src/main/scala/exe_09/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector map (_.toLong)

    val data_path = "src/main/scala/exe_09/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector map (_.toLong)

    def check_summing_pair(target_value: Long, value_set: Set[Long]): Boolean = {
      val result = value_set find { x =>
        val other = target_value - x
        (value_set contains other) && (other != x)
      }
      result match {
        case Some(value) => false
        case None => true
      }
    }

    def apply_check(values: Vector[Long]): Boolean = {
      val target_val = values.last
      val val_set = values.toSet - target_val
      check_summing_pair(target_val, val_set)
    }

    def find_invalid_entry(size_preamble: Int, data: Vector[Long]): Long = {
      // optimizations: add, remove to Set instead of recreating each time
      val windows = data.sliding(size_preamble + 1)  // uses Iterators -> no unnesseary work
      val result = (windows find apply_check)
      result match {
        case Some(vec) => vec.last
        case None => 0
      }
    }

    def find_encrpyt_weakness(invalid_entry: Long, data: Vector[Long]): Long = {
      // Efficient solution would be a dynamic window that adds and deletes elements
      // to it (at the start or end) depending weather it overstates or understates
      // the the invalid_entry (as a sum of the total window).
      //  given the lazy evaluation efficiency is ok though

      val all_sizes = (2 to data.size).view flatMap { x => data.sliding(x)}
      val result = all_sizes find { x => x.sum == invalid_entry }
      val result_vals: List[Long] = result match {
        case Some(vec) => List(vec.min, vec.max)
        case None => List(0,0)
      }
      result_vals.sum
    }

    println(find_invalid_entry(5, test_data))
    val result_1 = find_invalid_entry(25, data)
    println(result_1)
    println(find_encrpyt_weakness(127, test_data))
    println(find_encrpyt_weakness(result_1, data))

    // END
  }
}

