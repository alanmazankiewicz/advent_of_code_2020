package exe_02

import scala.io.Source


object Exe_02 {
  def main(args: Array[String]): Unit = {

    // val data_path = "src/main/scala/exe_02/data.txt"
    // val raw_data = Source.fromFile(data_path).getLines.toList
    // val data = raw_data.map(_.toInt)
    val test_case = "1-3 a: abcde"

    def parse_string(password_policy: String): Array[String] = {
      var tmp = password_policy.split(": ")
      val password = tmp[1]
      tmp
    }

    parse_string(test_case)

  }
}
