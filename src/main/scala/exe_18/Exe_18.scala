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

    def evaluate_expression(expression: String): Long = {
      val parsed = parse_expression(expression).toList

      def helper(ele: String): Long = {
        if (ele.length > 1) evaluate_expression(ele)
        else ele.toLong
      }

      def reduce(value: Long, operation: String, parsed: List[String]): Long = {
        val op_func = { (x:Long, y:Long) => if (operation == "+") x + y else x * y}
        val evaluated = op_func(value, helper(parsed.head))
        if (parsed.tail == Nil) evaluated
        else
          {
            val rest = parsed.tail
            val next_operation = rest.head
            reduce(evaluated, next_operation, rest.tail)
          }
      }
      reduce(0L, "+", parsed)
    }

    def evaluate_expression_pt2(expression: String): Long = {
      val parsed = parse_expression(expression).toList

      def helper(ele: String): Long = {
        if (ele.length > 1) evaluate_expression_pt2(ele)
        else ele.toLong
      }

      def reduce_sum(value: Long, operation: String, parsed: List[String], multiplications: Vector[Long]): Vector[Long] = {
       val (new_multi, new_value) =  if (operation == "*") {
         val n_value = helper(parsed.head)
         (multiplications :+ n_value, n_value)
        }
        else {
         val n_value = value + helper(parsed.head)
         (multiplications.dropRight(1) :+ n_value, n_value)
        }

        if (parsed.tail == Nil) new_multi
        else
        {
          val rest = parsed.tail
          val next_operation = rest.head
          reduce_sum(new_value, next_operation, rest.tail, new_multi)
        }
      }
      reduce_sum(0L, "+", parsed, Vector()).product
    }


    def run(data: Vector[String], evaluate_expression: String => Long): Long = {
      (data map evaluate_expression).sum
    }

    println(run(data, evaluate_expression))
    println(run(data, evaluate_expression_pt2))


    // END
  }
}
