package exe_12

import scala.io.Source
import scala.math.abs

object Exe_12 {
  def main(args: Array[String]): Unit = {

    val test_data_path = "src/main/scala/exe_12/test_data.txt"
    val test_data = Source.fromFile(test_data_path).getLines.toList

    val data_path = "src/main/scala/exe_12/test_data.txt"
    val data = Source.fromFile(data_path).getLines.toList

    val compass = Map(
      0 -> 'N',
      90 -> 'E',
      180 -> 'S',
      270 -> 'W'
    )

    def turn(ship: (Int, Int, Int), action: Char, value: Int): (Int, Int, Int) = {
      val (x, y, orient) = ship
      val direction = if(action == 'L') -1 else 1
      val turn_direction = value * direction
      (x, y, (orient + turn_direction + 360) % 360)
    }

    def move_compass(ship: (Int, Int, Int), action: Char, value: Int): (Int, Int, Int) = {
      val (x, y, orient) = ship
      action match {
        case 'N' => (x + value, y, orient)
        case 'S' => (x - value, y, orient)
        case 'E' => (x, y + value, orient)
        case 'W' => (x, y - value, orient)
      }
    }

    def move_forward(ship: (Int, Int, Int), action: Char, value: Int): (Int, Int, Int) = {
      // action not necessary but kept for consistency
      val (x, y, orient) = ship
      val comp_direction = compass(orient)
      move_compass(ship, comp_direction, value)
    }

    def parse_instruct(raw_instruct: String): (Char, Int) = {
      val splitted = raw_instruct.splitAt(1)
      (splitted._1(0), splitted._2.toInt)
    }

    def move(ship: (Int, Int, Int), raw_instruct: String): (Int, Int, Int) = {
      val (action, value) = parse_instruct(raw_instruct)
      action match {
        case 'N' | 'S' | 'E' | 'W' => move_compass(ship, action, value)
        case 'L' | 'R' => turn(ship, action, value)
        case 'F' => move_forward(ship, action, value)
      }
    }

    def set_sail(raw_instructs: List[String]): (Int, Int, Int) = {
      (raw_instructs foldLeft (0, 0, 90))(move)
    }

    def evaluate_ship(ship: (Int, Int, Int)): Int = {
      abs(ship._1) + abs(ship._2)
    }

    println(evaluate_ship(set_sail(test_data)))
    println(evaluate_ship(set_sail(data)))

  // END
  }
}
