package Exe_15

object Exe_15 {

  def main(args: Array[String]): Unit = {
    val data = (Vector(6,19,0,5,7,13,1).zipWithIndex map { x => x._1 -> (x._2 + 1) }).toMap

    def mem_game(no_rounds: Int): Int = {

      def loop(memory: Map[Int, Int], round: Int, last_value: Int): Int = {
        if (round > no_rounds) last_value
        else {
          val result = memory.get(last_value) match {
            case Some(prev_round) => round - prev_round -1
            case None => 0
          }
          loop(memory.updated(last_value, round-1), round+1, result)
        }
      }

      def init_game(memory: Map[Int, Int], round: Int, last_value: Int):Int = loop(memory, round +1, 0)

      init_game(data, data.size +1, data.last._1)
    }

    println(mem_game(2020)) // part 1
    println(mem_game(30000000)) // part 2
    // END
  }
}
