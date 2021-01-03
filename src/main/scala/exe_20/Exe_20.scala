package exe_20

import scala.io.Source
import scala.collection.mutable

object Exe_20 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_20/test_data.txt"
    val data = Source.fromFile(path).getLines.mkString

    class Tile(val id: Long, val upper: String, val right: String, val lower: String, val left: String) { // could be parsed to booleans for size optimization
      def rotate(): Tile = {
        val new_upper = this.left.reverse
        val new_right = this.upper
        val new_lower = this.right.reverse
        val new_left = this.lower
        new Tile(this.id, new_upper, new_right, new_lower, new_left)
      }

      def flip(): Tile = {
        val new_upper = this.lower
        val new_right = this.right.reverse
        val new_lower = this.upper
        val new_left = this.left.reverse
        new Tile(this.id, new_upper, new_right, new_lower, new_left)
      }

      val get_edges: List[String] = {
        List(this.upper, this.right, this.lower, this.left)
      }

      val edge_map: Map[String, String] = {
        (List("upper", "right", "lower", "left") zip this.get_edges).toMap
      }
    }

    def parse(data: String): List[Tile] = {
      val splitted = data.split("Tile ") filter { x => x != "" } // could be done better

      def parse_tile(raw_tile: String): Tile = {
        val splitted = raw_tile.split(":")
        val id = splitted(0).toLong
        val two_d = splitted(1).grouped(10).toVector

        val upper = two_d.head
        val lower = two_d.last

        val trans_two_d = two_d.transpose
        val left = trans_two_d.head.mkString
        val right = trans_two_d.last.mkString

        new Tile(id, upper, right, lower, left)
      }

      (splitted map parse_tile).toList
    }

    def find_orient_neigtbour(tiles: List[Tile])(tile: Tile): List[(String, Tile)] = {

      // it is absolultey clear that if two edge pattern match irrespective of orientation these tiles definetly match!
      // An edge cannot match more than one other match irrespective of orientation!!!
      def get_any_edge(cur_tile_edge: String): Option[Tile] = {
        tiles find { x =>
          (x.lower == cur_tile_edge || x.upper == cur_tile_edge || x.left == cur_tile_edge || x.right == cur_tile_edge ||
            x.lower.reverse == cur_tile_edge || x.upper.reverse == cur_tile_edge || x.left.reverse == cur_tile_edge || x.right.reverse == cur_tile_edge) &&
            x.id != tile.id
        }
      }


      val res: List[(String, Tile)] = (for {(k, v) <- tile.edge_map
                                            res <- get_any_edge(v)
                                            } yield (k, res)).toList
      ("-", tile) :: res
    }

    def find_corners(tiles: List[Tile]): List[Long] = {
      tiles map find_orient_neigtbour(tiles) filter { x => x.length == 3 } map { x => x.head._2.id }

    }

    val parsed_data = parse(data)
    //    val result_1 = find_corners(parsed_data).product
    //    println(result_1)

    // part_2

    val reveresed_direction = Map(
      "upper" -> "lower", "lower" -> "upper", "left" -> "right", "right" -> "left"
    )

    def get_correct_orient_neightbour(neighbour_tiles: List[(String, Tile)]): (Long, List[(Tile, String, Int, Boolean)]) = {
      val tile = neighbour_tiles.head

      def match_tiles(main_tile_edge: String, other_tile: Tile, direction: String, rotations: Int): Option[(Tile, String, Int, Boolean)] = {
        if (rotations == 4) None
        else if (other_tile.edge_map(reveresed_direction(direction)) == main_tile_edge) Option((other_tile, direction, rotations, false))
        else if (other_tile.flip().edge_map(reveresed_direction(direction)) == main_tile_edge) Option((other_tile.flip(), direction, rotations, true))
        else match_tiles(main_tile_edge, other_tile.rotate(), direction, rotations + 1)
      }

      val good_neighboors = (for {
        (direction, main_tile_edge) <- tile._2.edge_map
        (other_dir, other_tile) <- neighbour_tiles.tail
        if direction == other_dir
        result <- match_tiles(main_tile_edge, other_tile, direction, 0)} yield result).toList

      (tile._2.id, good_neighboors)
    }

    def build_map_skeleton(initial_tile: Tile)(implicit tiles: List[Tile]): List[(Long, List[(Tile, String, Int, Boolean)])] = {
      val visited: mutable.Set[Long] = mutable.Set()

      def dynamic_search(tile: Tile)(implicit tiles: List[Tile]): List[(Long, List[(Tile, String, Int, Boolean)])] = {
        val tmp = get_correct_orient_neightbour(find_orient_neigtbour(tiles)(tile))
        val cur_results = (tmp._1, tmp._2 filter { x => !visited.contains(x._1.id) })
        visited += tile.id
        val final_res: List[(Long, List[(Tile, String, Int, Boolean)])] = (for {
          res <- cur_results._2
          if !visited.contains(res._1.id)
        } yield dynamic_search(res._1)) flatMap { x => x }
        cur_results :: final_res
      }
      dynamic_search(initial_tile)
    }


    val test = build_map_skeleton(parsed_data(1).flip())(parsed_data)


    println("as")
    //END
  }
}
