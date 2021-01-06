package exe_22

import scala.collection.mutable
import scala.io.Source

object Exe_22 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_22/"
    val raw_p1 = Source.fromFile(path + "player_1.txt").getLines.toList
    val raw_p2 = Source.fromFile(path + "player_2.txt").getLines.toList

    class Player(raw_deck: List[String]) {
      private val deck = mutable.Queue(raw_deck map { x => x.toInt }: _*)

      def draw(): Int = deck.dequeue()

      def add_deck(win_card: Int, loose_card: Int): Unit = {
        deck += win_card; deck += loose_card
      }

      def notEmpty(): Boolean = deck.nonEmpty

      def get_score(): Int = {
        val tmp_deck = deck.toList.reverse
        (tmp_deck.zipWithIndex map { x => x._1 * (x._2 + 1) }).sum
      }
    }

    def play_round(fst: Player, sec: Player): Unit = {
      val fst_card = fst.draw()
      val sec_card = sec.draw()

      if (fst_card > sec_card) fst.add_deck(fst_card, sec_card)
      else sec.add_deck(sec_card, fst_card)
    }

    def play_match(fst: Player, sec: Player): Player = {
      while(fst.notEmpty() && sec.notEmpty()){
        play_round(fst, sec)
      }
      if (fst.notEmpty()) fst
      else sec
    }

    val fst = new Player(raw_p1)
    val sec = new Player(raw_p2)
    val winning_score = play_match(fst, sec).get_score()
    println(winning_score)

    // END
  }
}