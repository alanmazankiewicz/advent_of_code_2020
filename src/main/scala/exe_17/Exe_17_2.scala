package exe_17

import scala.io.Source

object Exe_17_2 {

  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_17/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toVector

    val data_path = "src/main/scala/exe_17/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toVector

    def parse_input(data: Vector[String]): Set[(Int, Int, Int, Int)] = {
      (data.zipWithIndex flatMap { y => y._1.zipWithIndex filter { x => x._1 == '#' } map { x => (x._2, y._2, 0, 0) } }).toSet
    }

    def get_surrounding(cube: (Int, Int, Int, Int)): List[(Int, Int, Int, Int)] = {
      val (x,y,z,w) = cube
      (for {
        a <- -1 to 1
        b <- -1 to 1
        c <- -1 to 1
        d <- -1 to 1
        if (a,b,c,d) != (0,0,0,0)
      } yield (a+x, b+y, c+z, d+w)).toList
    }

    def deactivate(implicit active_cubes: Set[(Int, Int, Int, Int)]): Set[(Int, Int, Int, Int)] = {

      def deactivate_cube(cube: (Int, Int, Int, Int)): Boolean = {
        val surround_count = get_surrounding(cube) count { x => active_cubes.contains(x) }
        (surround_count == 2) || (surround_count == 3)
      }
      active_cubes filter deactivate_cube
    }

    def activate(implicit active_cubes: Set[(Int, Int, Int, Int)]): Set[(Int, Int, Int, Int)] = {
      val histogram = active_cubes.toList flatMap get_surrounding filter { x => !active_cubes.contains(x) } groupBy(x => x) map { x => (x._1, x._2.length) }
      (histogram filter { case (cube, count) => count == 3 } map { case (cube, value) => cube }).toSet
    }

    def cycle(implicit active_cubes: Set[(Int, Int, Int, Int)]): Set[(Int, Int, Int, Int)] = {
      deactivate union activate
    }

    def run(n_cyc: Int, active_cubes: Set[(Int, Int, Int, Int)]): Int = {
      if (n_cyc == 0) active_cubes.size
      else run(n_cyc-1, cycle(active_cubes))
    }

    println(run(6, parse_input(test_data)))
    println(run(6, parse_input(data)))


    // END
  }
}
