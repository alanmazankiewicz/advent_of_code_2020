package exe_20

import scala.io.Source

object Exe_20 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_20/data.txt"
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

      def get_edges(): List[String] = {
        List(this.upper, this.right, this.lower, this.left)
      }
    }

    def parse(data: String): List[Tile] = {
      val splitted = data.split("Tile ") filter { x => x != "" }  // could be done better

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


//    def get_upper(implicit cur_tile: Tile, tiles: List[Tile]): List[Tile] = {
//      tiles filter { x => x.lower == cur_tile.upper }
//    }
//
//    def get_right(implicit cur_tile: Tile, tiles: List[Tile]): List[Tile] = {
//      tiles filter { x => x.right == cur_tile.left }
//    }
//
//    def get_lower(implicit cur_tile: Tile, tiles: List[Tile]): List[Tile] = {
//      tiles filter { x => x.upper == cur_tile.lower }
//    }
//
//    def get_left(implicit cur_tile: Tile, tiles: List[Tile]): List[Tile] = {
//      tiles filter { x => x.left == cur_tile.right }
//    }

    def print_size[T](lst: List[T]): Unit = println(lst.length)

    def find_orient_neigtbour(tiles: List[Tile])(tile: Tile): List[Tile] = {

      // it is absolultey clear that if two edge pattern match irrespective of orientation these tiles definetly match!
      // An edge cannot match more than one other match irrespective of orientation!!!
      def get_any_edge(cur_tile_edge: String): List[Tile] = {
        tiles filter { x => (x.lower == cur_tile_edge || x.upper == cur_tile_edge || x.left == cur_tile_edge || x.right == cur_tile_edge ||
          x.lower.reverse == cur_tile_edge || x.upper.reverse == cur_tile_edge || x.left.reverse == cur_tile_edge || x.right.reverse == cur_tile_edge) &&
          x.id != tile.id }
      }

      val res = tile.get_edges() flatMap get_any_edge
      tile :: res
    }

    def find_corners(tiles: List[Tile]): List[Long] = {
      tiles map find_orient_neigtbour(tiles) filter { x => x.length == 3 } map { x => x.head.id }
    }

    val parsed_data = parse(data)
    val result_1 = find_corners(parsed_data).product
    println(result_1)

    val test = find_orient_neigtbour(parsed_data)(parsed_data(1))



    println("as")
    //END
  }
}
