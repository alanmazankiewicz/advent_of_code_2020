package exe_20

import scala.io.Source
import scala.collection.mutable
import scala.collection.immutable.SortedMap

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

    def parse(data: String): (List[Tile], Map[Long, Vector[String]]) = {
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

      def parse_inner_fields(data: String): (Long, Vector[String]) = {
        val splitted = data.split(":")
        val id = splitted(0).toLong
        val two_d = splitted(1).grouped(10).toVector
        val tmp = two_d.drop(1).dropRight(1)
        val fin = tmp.transpose.drop(1).dropRight(1).transpose map { x => x.mkString }
        (id, fin)
      }

      val tiles = (splitted map parse_tile).toList
      val inner_fields = (splitted map parse_inner_fields).toMap
      (tiles, inner_fields)
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

    val (parsed_data, parsed_fields) = parse(data)
    //    val result_1 = find_corners(parsed_data).product
    //    println(result_1)

    // part_2

    val reveresed_direction = Map(
      "upper" -> "lower", "lower" -> "upper", "left" -> "right", "right" -> "left"
    )

    val sea_monster = List(
      (1, -18), (1, -13), (1, -12), (1, -7), (1, -6), (1, -1), (1, 0), (1,1),
      (2, -17), (2, -14), (2, -11), (2, -8), (2, -5), (2, -2)
    ) // TODO May contain error

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

    def build_grid(map_skeleton: List[(Long, List[(Tile, String, Int, Boolean)])]): Map[(Int, Int), Long] = {
      val skeleton_head = map_skeleton.head
      val grid: mutable.Map[Long, (Int, Int)] = mutable.Map(skeleton_head._1 -> (0, 0))

      def new_coord(cur_coord: (Int, Int), map_info: (Tile, String, Int, Boolean)): Unit = {
        val (x, y) = cur_coord
        val tile_id = map_info._1.id
        val next_coord = map_info._2 match {
          case "upper" => (x - 1, y)
          case "right" => (x, y + 1)
          case "lower" => (x + 1, y)
          case "left" => (x, y - 1)
        }
        grid += (tile_id -> next_coord)
      }

      map_skeleton map { x => x._2 map { y => new_coord(grid(x._1), y) } } // Not very functional :D -> could be done with foldLeft

      for {(k, v) <- grid.toMap} yield (v, k)
    }

    def rotate_image(image: Vector[String]): Vector[String] = {
      val out = (image map (_.toArray)).toArray

      val size = image.length;

      {
        (0 until size).map { i =>
          (0 until size).map { j =>
            out(i)(j) = image(size - j - 1)(i)
          }
        }
      }
      (out map (_.mkString)).toVector
    }

    def flip_image(image: Vector[String]): Vector[String] = {
      (image.transpose map (x => x.reverse)).transpose map (x => x.mkString)
    }

    def adjust_inner_fields(inner_fields: Map[Long, Vector[String]], map_skeleton: List[(Long, List[(Tile, String, Int, Boolean)])]): Map[Long, Vector[String]] = {
      val adjustment_data: List[(Tile, String, Int, Boolean)] = map_skeleton flatMap {x => x._2}
      val base_id = map_skeleton.head._1
      val base_field = flip_image(inner_fields(base_id)) // TODO testing -> dont flip

      def adjust_field(id: Long, rotations: Int, flip: Boolean): (Long, Vector[String]) = {
        val field = inner_fields(id)
        val res = (0 until rotations).foldLeft(field)((x, y) => rotate_image(x))
        if (flip) (id, flip_image(res))
        else (id, res)
      }

      (adjustment_data map { x => adjust_field(x._1.id, x._3, x._4) }).toMap + (base_id -> base_field)
    }

    def build_image(grid: Map[(Int, Int), Long], final_fields: Map[Long, Vector[String]]): Vector[String] = {
      val grouped_map = (grid groupBy { case (k, v) => k._1 }) map { case (k,v) => k -> (v.toList.sortBy(x => x._1._2)  map { x => x._2 }) }
      val grouped = SortedMap(grouped_map.toSeq:_*) map { case (k, v) => v }  // TODO should be sorted now but may be problem
      val grouped_fields = grouped map { x => x map final_fields }

      def join_fields(field_1: Vector[String], field_2: Vector[String]): Vector[String] = {
        (field_1 zip field_2) map { x => x._1 + x._2}
      }

      val joined_rows = grouped_fields map { x => x reduce join_fields }
      val result = (joined_rows map {x => x.transpose map { y => y.mkString }} reduce join_fields).transpose map { x => x.mkString }

      result
    }


    def check_sea_monsters(image: Vector[String]): Option[Vector[Vector[(Int, Int)]]] = {

      def check_location(pos: (Int, Int)): Option[Vector[(Int, Int)]] = {
        val absolut_locs = sea_monster map { x => (pos._1 + x._1, pos._2 + x._2) }
        if (absolut_locs exists { x => x._1 < 0 || x._2 < 0 || x._1 >= image.length || x._2 >= image.length }) None
        else {
          if (absolut_locs map { x => image(x._1)(x._2)} forall { x => x == '#' }) Option(absolut_locs.toVector)
          else None
        }
      }

      val potential_monsters = for {
        x <- image.indices
        y <- image(0).indices
        if image(x)(y) == '#'
      } yield (x,y)

      val res = (for {
        x <- potential_monsters
        res <- check_location(x)
      } yield res)

      if (res.isEmpty) None
      else Option(res.toVector)
    }

    def run(tiles: List[Tile], inner_fields: Map[Long, Vector[String]]): Int = {
      // val corner = find_corners(tiles)(0)
      val corner = tiles(1).flip() // TODO testing
      val map_skeleton = build_map_skeleton(corner)(parsed_data)
      val grid = build_grid(map_skeleton)
      val adjusted_inner_fields = adjust_inner_fields(inner_fields, map_skeleton)
      val image = build_image(grid, adjusted_inner_fields)
      val sea_monsters = check_sea_monsters(image) // TODO testing: must be another func that rotates and flips in case of no monsters

      // TODO testing
      sea_monsters match {
        case Some(i) => i.length
        case None => 0
      }
    }


    // TODO we must start with top left pic? or we must start with some pick thats oriented top left, or it does not matter?
    val test = build_map_skeleton(parsed_data(1).flip())(parsed_data)
    val test_grid = build_grid(test)
    val test3 = adjust_inner_fields(parsed_fields, test)
    val test4 =  build_image(test_grid, test3)

    val big_test = run(parsed_data, parsed_fields)


    println("as")
    //END
  }
}
