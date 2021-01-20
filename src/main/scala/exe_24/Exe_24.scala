package exe_24

import scala.io.Source

object Exe_24 {
  def main(args: Array[String]): Unit = {
    val path = "src/main/scala/exe_24/data.txt"
    val data = Source.fromFile(path).getLines.toList

    def parseData(inp: List[String]): List[List[String]] = {

      def parseLine(line: String): List[String] = {

        def loop(tail: List[Char]): List[String] = {
          if (tail == Nil) Nil
          else {
            val head = tail.head
            if(head == 'e' || head == 'w') head.toString :: loop(tail.tail)
            else {
              val next = tail.tail
              (head :: next.head :: Nil).mkString :: loop(next.tail)
            }
          }
        }
        loop(line.toList)
      }
      inp.map(parseLine)
    }

    val move: Map[String, (Int, Int)] = Map(
      "e" -> (2, 0),
      "w" -> (-2, 0),
      "se" -> (1, -1),
      "sw" -> (-1, -1),
      "ne" -> (1, 1),
      "nw" -> (-1, 1)
    )

    def delta(start_pos: (Int, Int), movement: String): (Int, Int) = {
      val (x, y) = start_pos
      val (delta_x, delta_y) = move(movement)
      (x + delta_x, y + delta_y)
    }

    def flipTile(directions: List[String]): (Int, Int) = {
      directions.foldLeft((0,0))(delta)
    }

    def getBlack(inp: List[List[String]]): Int = {
      inp.map(flipTile).groupBy(identity).map(x => x._2.length).count(x => (x % 2) == 1)
    }

    val res_1 = getBlack(parseData(data))

    println(res_1)

    // part 2

    def init_floor(inp: List[List[String]]): Map[(Int, Int), Char] = {
      def getColor(count: Int): Char = if((count % 2) == 1) 'b' else 'w'
      inp.map(flipTile).groupBy(identity).map(x => (x._1, x._2.length)).map(x => (x._1, getColor(x._2)))
    }

    def getAdjacent(tile: (Int, Int)): List[(Int, Int)] = {
      val deltas = List((0, 1), (0, -1), (1, -1), (-1, -1), (1, 1), (-1, 1))
      deltas.map( x => (x._1 + tile._1, x._2 + tile._2))
    }

    def changeTile(tile: ((Int, Int), Char), tiles: Map[(Int, Int), Char]): ((Int, Int), Char) = {
      val (pos, col) = tile
      val adjTiles = getAdjacent(pos)
      val tmp = adjTiles.map(x => tiles.getOrElse(x, '_')).groupBy(identity).find(x => x._1 == 'b')
      val black_count = tmp match {
        case Some(black) => black._2.length
        case None => 0
      }

      col match {
        case 'b' if black_count == 0 || black_count > 2 => (pos, 'w')
        case 'w' if black_count == 2 => (pos, 'b')
        case _ => (pos, col)
      }
    }

    def forDays(n: Int, tiles: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      if (n == 0) tiles
      else {
        val other_tiles = tiles.flatMap(x => getAdjacent(x._1)).toList.filter(x => !tiles.contains(x)).map(x => (x, 'w')).toMap
        val full_tiles = tiles ++ other_tiles
        val new_tiles: Map[(Int, Int), Char] = full_tiles.map(x => changeTile(x, full_tiles))
        forDays(n-1, new_tiles)
      }
    }

    def getResult(tiles: Map[(Int, Int), Char]): Int = {
      tiles.toList.groupBy(x => x._2).filter(x => x._1 == 'b').head._2.length
    }

    def part2(inp: List[String]): Int = {
      val parsed = parseData(inp)
      val initial_floor = init_floor(parsed)
      val final_tiles = forDays(100, initial_floor)
      getResult(final_tiles)
    }

    val res_2 = part2(data)
    println(res_2)

    println("Asd")

    // END
  }
}
