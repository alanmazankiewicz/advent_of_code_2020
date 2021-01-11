package exe_23

import scala.collection.mutable
import scala.io.Source

object Exe_23 {
  def main(args: Array[String]): Unit = {
    val data = "624397158".map(_.asDigit).toArray

    def next_pos(cur_pos: Int): Int = (cur_pos + 1) % data.length

    def split_cups(current_cup_pos: Int, cups: Array[Int]): (Array[Int], Array[Int]) = {

      def create_pick_up_range(cur_pos: Int, i: Int = 3, pick_up_range: Set[Int] = Set()):Set[Int] = {
        if (i == 0) pick_up_range
        else {
          val nxt = next_pos(cur_pos)
          val new_range = pick_up_range + nxt
          create_pick_up_range(nxt, i -1, new_range)
        }
      }

      val remaining_cups: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer()
      val pick_up: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer()
      val pick_up_range = create_pick_up_range(current_cup_pos)
      var idx = current_cup_pos

      do {
        if(pick_up_range.contains(idx)) {
          pick_up += cups(idx)
          idx = next_pos(idx)
        }
        else {
          remaining_cups += cups(idx)
          idx = next_pos(idx)
        }
      } while(idx != current_cup_pos)

      (remaining_cups.toArray, pick_up.toArray)
    }

    def merge_cups(destination_pos: Int, remaining_cups: Array[Int], pick_up: Array[Int]): Array[Int] = {
      val res: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer()
      var i = 0
      var j = 0

      while(i <= destination_pos){
        res += remaining_cups(i)
        i += 1
      }

      while(j < 3) {
        res += pick_up(j)
        j += 1
      }

      while(i < remaining_cups.length){
        res += remaining_cups(i)
        i += 1
      }

      res.toArray
    }

    def get_destination_pos(current_val: Int, remaining_cups: Array[Int], pick_up: Array[Int]): Int = {
      var destination_val = current_val
      var break = true
      val min_val = remaining_cups.min

      while(break){
        destination_val -= 1
        if(destination_val < min_val) destination_val = remaining_cups.max
        if(!pick_up.contains(destination_val)) break = false
      }
      remaining_cups.indexOf(destination_val)
    }

    def transform_res(inp: Array[Int]): Array[Int] = {
      val one_idx = inp.indexOf(1)
      var idx = next_pos(one_idx)
      val res = mutable.ArrayBuffer[Int]()

      while(idx != one_idx) {
        res += inp(idx)
        idx = next_pos(idx)
      }
      res.toArray
    }

    def part_1(inp: Array[Int], iterations: Int, current_pos: Int = 0): String = {

      if(iterations == 0) transform_res(inp).mkString
      else {
        val current_val = inp(current_pos)
        val next_val = inp((current_pos + 4) % inp.length)
        val (remaining_cups, pick_up) = split_cups(current_pos, inp)
        val destination_pos = get_destination_pos(current_val, remaining_cups, pick_up)
        val res = merge_cups(destination_pos, remaining_cups, pick_up)
        val new_pos = res.indexOf(next_val)
        part_1(res, iterations -1, new_pos)
      }
    }

    println(part_1(data, 100))

    // END
  }
}
