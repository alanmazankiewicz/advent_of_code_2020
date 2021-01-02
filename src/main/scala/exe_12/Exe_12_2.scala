package exe_12

import scala.io.Source
import scala.math.abs

object Exe_12_2 {
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

    def turn(ship: (Int, Int, Int, Int), action: Char, value: Int): (Int, Int, Int, Int) = {
      val (ship_x, ship_y, way_x, way_y) = ship
      val adjusted_val = if (action == 'L' && value != 180) (value + 180) % 360 else value

      adjusted_val match {
        case 90 => (ship_x, ship_y, way_y, -way_x)
        case 180 => (ship_x, ship_y, -way_x, -way_y)
        case 270 => (ship_x, ship_y, -way_y, way_x)
      }
    }

    def move_waypoint(ship: (Int, Int, Int, Int), action: Char, value: Int): (Int, Int, Int, Int) = {
      val (ship_x, ship_y, way_x, way_y) = ship
      action match {
        case 'N' => (ship_x, ship_y, way_x, way_y + value)
        case 'S' => (ship_x, ship_y, way_x, way_y - value)
        case 'E' => (ship_x, ship_y, way_x  + value, way_y)
        case 'W' => (ship_x, ship_y, way_x - value, way_y)
      }
    }

    def move_forward(ship: (Int, Int, Int, Int), action: Char, value: Int): (Int, Int, Int, Int) = {
      val (ship_x, ship_y, way_x, way_y) = ship
      (ship_x + way_x * value, ship_y + way_y * value, way_x, way_y)
    }

    def parse_instruct(raw_instruct: String): (Char, Int) = {
      val splitted = raw_instruct.splitAt(1)
      (splitted._1(0), splitted._2.toInt)
    }

    def move(ship: (Int, Int, Int, Int), raw_instruct: String): (Int, Int, Int, Int) = {
      val (action, value) = parse_instruct(raw_instruct)
      action match {
        case 'N' | 'S' | 'E' | 'W' => move_waypoint(ship, action, value)
        case 'L' | 'R' => turn(ship, action, value)
        case 'F' => move_forward(ship, action, value)
      }
    }

    def set_sail(raw_instructs: List[String]): (Int, Int, Int, Int) = {
      (raw_instructs foldLeft(0, 0, 10, 1)) (move)
    }

    def evaluate_ship(ship: (Int, Int, Int, Int)): Int = {
      abs(ship._1) + abs(ship._2)
    }

    println(evaluate_ship(set_sail(test_data)))
    println(evaluate_ship(set_sail(data)))

    // END
  }
}
