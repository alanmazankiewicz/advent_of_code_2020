package exe_01

import scala.io.Source


object Exe_01 {
  def main(args: Array[String]): Unit = {

    val data_path = "src/main/scala/exe_01/test_data.txt"
    val raw_data = Source.fromFile(data_path).getLines.toList
    val data = raw_data.map(_.toInt)
    val test_data = (1 to 20).toList

    def check_duplicates(values: List[Int]): Unit = {
      val value_set = values.toSet
      if (value_set.size != values.size) {
        throw new Exception("Not implemented for duplicates in values")
      } else {
        None
      }
    }

    check_duplicates(data)

    def mul_summing_pair(target_value: Int, value_set: Set[Int]): Int = {

      val result = value_set find { x =>
        val other = target_value - x
        (value_set contains other) && (other != x)
      }
      result match {
        case Some(value) => value * (target_value - value)
        case None => -1
      }
    }

    val result_1 = mul_summing_pair(2020, data.toSet)
    println(result_1)

    def mul_summing_triple(target_value: Int, values: List[Int], value_set: Set[Int]): Int = {
      if (values.isEmpty) -1
      else {
        val head = values.head
        val current_rest = target_value - head
        val rest_set = value_set - head
        val result_rest_list = mul_summing_pair(current_rest, rest_set)
        if (result_rest_list != -1)
          head * result_rest_list
        else mul_summing_triple(target_value, values.tail, rest_set)
      }
    }

    val result_2 = mul_summing_triple(2020, data, data.toSet)
    println(result_2)
  }
}
