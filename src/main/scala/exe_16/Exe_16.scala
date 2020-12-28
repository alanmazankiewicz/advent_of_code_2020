package exe_16

import scala.io.Source

object Exe_16 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_16/"
    val test_fields = Source.fromFile(path + "test_fields.txt").getLines.toVector
    val test_nearby_tickets = Source.fromFile(path + "test_nearby_tickets.txt").getLines.toVector

    val fields = Source.fromFile(path + "fields.txt").getLines.toVector
    val nearby_tickets = Source.fromFile(path + "nearby_tickets.txt").getLines.toVector

    def parse_fields(fields: Vector[String]): Set[Int] = {
      val splitted_raw = fields flatMap { x => x.split(" ") } filter { x => x.matches("\\d+-\\d+") }
      (splitted_raw map { x => x.split("-") } flatMap { x => x(0).toInt to x(1).toInt }).toSet
    }

    def parse_nearby_tickets(tickets: Vector[String]): List[Set[Int]] = {
      (tickets map { x => x.split(",") map { y => y.toInt } } map (x => x.toSet)).toList
    }

    def error_rate(parsed_fields: Set[Int], parsed_n_tickets: List[Set[Int]]): Int = {
      val invalids: List[Int] = parsed_n_tickets flatMap { set => set.diff(parsed_fields) }
      invalids.sum
    }

    println(error_rate(parse_fields(fields), parse_nearby_tickets(nearby_tickets)))

    // part 2

    def filter_invalids(parsed_fields: Set[Int], parsed_n_tickets: List[Set[Int]]): List[List[Int]] = {
      parsed_n_tickets filter { set => set.subsetOf(parsed_fields) } map { x => x.toList }
    }

    def parse_fields_v2(fields: Vector[String]): List[(String, Set[Int])] = {
      val splitted = (fields map { x => x.split(": ") } map { x => (x(0), x(1)) }).toList
      val tmp = splitted map { x => (x._1, x._2.split(" ") filter { x => x.matches("\\d+-\\d+") }) }
      tmp map { y => (y._1, (y._2 map { x => x.split("-") } flatMap { x => x(0).toInt to x(1).toInt }).toSet) }
    }

    def find_fields(valid_tickets: List[List[Int]], field_map: List[(String, Set[Int])]): List[String] = {
      val fields = valid_tickets.transpose

      def helper(res: Option[(String, Set[Int])]): String = {
        res match {
          case Some(x) => x._1
        }
      }

      fields map { fld => field_map find { mp => fld forall { x => mp._2.contains(x) } } } map helper
    }


    def run_v2(fields: Vector[String], nearby_tickets: Vector[String]): List[String] = {
      val valid_tickets = filter_invalids(parse_fields(fields), parse_nearby_tickets(nearby_tickets))
      find_fields(valid_tickets, parse_fields_v2(fields))
    }

    val test = run_v2(test_fields, test_nearby_tickets)

    println("s")

    // END
  }
}
