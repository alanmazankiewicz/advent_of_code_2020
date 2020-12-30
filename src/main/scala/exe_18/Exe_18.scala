package exe_18

import scala.io.Source

object Exe_18 {
  def main(args: Array[String]): Unit = {

//    val test_data_path = "src/main/scala/exe_18/test_data.txt"
//    val test_data = Source.fromFile(test_data_path).getLines.toVector
//
    val data_path = "src/main/scala/exe_18/data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def parse_expression(expression: String): Vector[String] = {

      def loop(rest: List[Char], result: Vector[String], expression_builder: StringBuilder, par_count: Int): Vector[String] = {
        if (rest == Nil) result
        else {
          rest.head match {
            case ' ' => loop(rest.tail, result, expression_builder, par_count)
            case '(' if par_count == 0 => loop(rest.tail, result, expression_builder, par_count + 1)
            case '(' => loop(rest.tail, result, expression_builder += rest.head, par_count + 1)
            case ')' if par_count == 1 => loop(rest.tail, result :+ expression_builder.toString, new StringBuilder(), par_count - 1)
            case ')' => loop(rest.tail, result, expression_builder += rest.head, par_count - 1)
            case x if par_count > 0 => loop(rest.tail, result, expression_builder += rest.head, par_count)
            case x => loop(rest.tail, result :+ rest.head.toString, expression_builder, par_count)
          }
        }
      }
      loop(expression.toList, Vector(), new StringBuilder(), 0)
    }

    def evaluate_expression(expression: String): String = {
      val parsed = parse_expression(expression).toList

      def helper(ele: String): String = {
        if (ele.length > 1) evaluate_expression(ele)
        else ele
      }

      def reduce(value: String, operation: String, parsed: List[String]): String = {
        val op_func = if (operation == "+") { (x:String ,y:String) => x.toLong + y.toLong} else { (x:String ,y:String) => x.toLong * y.toLong}
        val evaluated = op_func(value, helper(parsed.head)).toString
        if (parsed.tail == Nil) evaluated
        else
          {
            val rest = parsed.tail
            val next_operation = rest.head
            reduce(evaluated, next_operation, rest.tail)
          }
      }
      reduce("0", "+", parsed)
    }

    def run(data: Vector[String]): Long = {
      (data map evaluate_expression map { x => x.toLong }).sum
    }

    println(run(data))
    // println(run(data, parse_expression_pt2))
    // END
  }
}
