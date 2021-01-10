package exe_22

import scala.collection.mutable
import scala.io.Source

object Exe_22 {

  def main(args: Array[String]): Unit = {

    val path = "src/main/scala/exe_22/"
    val raw_p1 = Source.fromFile(path + "player_1.txt").getLines.toList
    val raw_p2 = Source.fromFile(path + "player_2.txt").getLines.toList

    class Player(val id: String) {
      var deck: mutable.Queue[Int] = mutable.Queue()

      def create_from_scratch(raw_deck: List[String]): Unit = {
        deck = mutable.Queue(raw_deck map { x => x.toInt }: _*)
      }

      def create_from_queue(new_deck: mutable.Queue[Int]): Unit = {
        deck = new_deck
      }

      def copy(size: Int): Player = {
        val new_player = new Player(this.id)
        new_player.create_from_queue(deck.take(size))
        new_player
      }

      def size(): Int = deck.size

      def draw(): Int = deck.dequeue()

      def add_deck(win_card: Int, loose_card: Int): Unit = {
        deck += win_card;
        deck += loose_card
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
      while (fst.notEmpty() && sec.notEmpty()) {
        play_round(fst, sec)
      }
      if (fst.notEmpty()) fst
      else sec
    }

    val fst = new Player("1")
    val sec = new Player("2")
    fst.create_from_scratch(raw_p1)
    sec.create_from_scratch(raw_p2)
    val winning_score = play_match(fst, sec).get_score()
    println(winning_score)

    // part_2

    def play_match_2(fst: Player, sec: Player): Player = {
      val mem: mutable.ArrayBuffer[mutable.Queue[Int]] = mutable.ArrayBuffer()

      def play_round(fst: Player, sec: Player): Unit = {
        mem += fst.deck.clone()
        val fst_card = fst.draw()
        val sec_card = sec.draw()

        if (fst.size() >= fst_card && sec.size() >= sec_card) {
          val winner = play_match_2(fst.copy(fst_card), sec.copy(sec_card))
          if (winner.id == "1") fst.add_deck(fst_card, sec_card)
          else sec.add_deck(sec_card, fst_card)
        }
        else {
          if (fst_card > sec_card) fst.add_deck(fst_card, sec_card)
          else sec.add_deck(sec_card, fst_card)
        }
      }

      while (fst.notEmpty() && sec.notEmpty()) {
        if(mem.contains(fst.deck)) return fst
        else play_round(fst, sec)
      }
      if (fst.notEmpty()) fst
      else sec
    }

    fst.create_from_scratch(raw_p1)
    sec.create_from_scratch(raw_p2)
    val score_2 = play_match_2(fst, sec).get_score()
    println(score_2)

    // END
  }
}