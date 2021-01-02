package exe_14

import scala.io.Source

object Exe_14 {

  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_14/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toList

    val data_path = "src/main/scala/exe_14/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toList

    def parseLine(line: String): Either[(Long, Long), String] = {
      val splitted = line.split(" = ")
      if (splitted(0) == "mask") Right(splitted(1))
      else {
        val idx = splitted(0).replaceAll("[^0-9]", "")
        Left((idx.toInt, splitted(1).toLong))
      }
    }

    def parseLong(mask: String) = java.lang.Long.parseLong(mask, 2)

    def parse_bitmask(mask: String): (Long, Long) = {

      def helper(for_ones: Boolean)(bit: Char): Char = {
        if (for_ones) if (bit == '1') '1' else '0'
        else if (bit == '0') '0' else '1'
      }

      val one_mask = mask map helper(true)
      val zero_mask = mask map helper(false)
      (parseLong(one_mask), parseLong(zero_mask))
    }

    def apply_bitmask(value: Long, bitmask: (Long, Long)): Long = {
      val tmp = value | bitmask._1
      tmp & bitmask._2
    }

    def run(program: List[String]): Long = {
      val mem: Map[Long, Long] = Map() // actully vector of 2**36 but given input very sparse
      val bitmask = (0L, 0L)


      def run_program(mem: Map[Long, Long], bitmask: (Long, Long), program: List[String]): Map[Long, Long] = {
        if (program == Nil) mem
        else {
          val instruct = parseLine(program.head)
          instruct match {
            case Right(raw_mask) => run_program(mem, parse_bitmask(raw_mask), program.tail)
            case Left((idx, value)) => run_program(mem + (idx -> apply_bitmask(value, bitmask)), bitmask, program.tail)
          }
        }
      }

      val result_mem = run_program(mem, bitmask, program)
      result_mem.foldLeft(0L)(_ + _._2)
    }

    println(run(data))

    // part 2

    def convert2binary(value: Long): String = {
      val bin = value.toBinaryString
      val prefix_size = 36 - bin.length
      ("0" * prefix_size) ++ bin // Could be done more efficiently
    }

    def convert_idx(idx: String, mask: String): String = {
      val zipped = idx zip mask

      def apply(bit_pair: (Char, Char)): Char = {
        val (idx_bit, mask_bit) = bit_pair
        if (mask_bit == '1') '1'
        else if (mask_bit == 'X') 'X'
        else idx_bit
      }
      (zipped map apply).mkString
    }

    def final_addresses(converted_idx: String): List[String] = {

      def convert_X(bit: Char, accum: List[List[Char]]): List[List[Char]] = {
        if (bit != 'X') accum map { x => bit :: x }
        else (accum map { x => '0' :: x }) ++ (accum map { x => '1' :: x })
      }

      val accum: List[List[Char]] = List(List())
      val reduced = converted_idx.foldRight(accum)(convert_X)
      reduced map { x => x.mkString }
    }

    def run_v2(program: List[String]): Long = {
      val mem: Map[Long, Long] = Map() // actully vector of 2**36 but given input very sparse
      val bitmask = ""


      def run_program(mem: Map[Long, Long], bitmask: String, program: List[String]): Map[Long, Long] = {
        if (program == Nil) mem
        else {
          val instruct = parseLine(program.head)
          instruct match {
            case Right(mask) => run_program(mem, mask, program.tail)
            case Left((idx, value)) => {
              val idxes: List[Long] = final_addresses(convert_idx(convert2binary(idx), bitmask)) map parseLong
              val new_memory = idxes.foldLeft(mem)({ (m, i) => m + (i -> value) })
              run_program(new_memory, bitmask, program.tail)
            }
          }
        }
      }
      val result_mem = run_program(mem, bitmask, program)
      result_mem.foldLeft(0L)(_ + _._2)
    }

    println(run_v2(data))
    // END
  }
}
