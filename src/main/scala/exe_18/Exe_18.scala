package exe_18

import scala.io.Source

object Exe_18 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_18/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_18/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector


    // test
    // END
  }
}
