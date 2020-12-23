import scala.annotation.tailrec
import scala.collection.mutable

object Day22 {

  def getDecks: (List[Int], List[Int]) = {
    val input = Utils.getInputLines
    val splitIndex = input.indexOf("")

    val deck1 = input.slice(1, splitIndex).map(_.toInt)
    val deck2 = input.slice(splitIndex + 2, input.length).map(_.toInt)

    (deck1, deck2)
  }

  @tailrec
  def playCombat(deck1: List[Int], deck2: List[Int]): List[Int] = {
    if (deck1.isEmpty) {
      deck2
    } else if (deck2.isEmpty) {
      deck1
    } else {
      val (card1, card2) = (deck1.head, deck2.head)

      val (new1, new2) = if (card1 > card2) {
        (deck1.drop(1).appendedAll(List(card1, card2)), deck2.drop(1))
      } else {
        (deck1.drop(1), deck2.drop(1).appendedAll(List(card2, card1)))
      }

      playCombat(new1, new2)
    }
  }

  def playRecursiveCombatGame(deck1: mutable.Queue[Int], deck2: mutable.Queue[Int]): (Boolean, mutable.Queue[Int]) = {
    val previousRounds = collection.mutable.Set[(mutable.Queue[Int], mutable.Queue[Int])]()

    while (deck1.nonEmpty && deck2.nonEmpty) {

      if (previousRounds.contains((deck1, deck2)) ) {
        return (true, mutable.Queue())
      } else {
        previousRounds.add((deck1.clone(), deck2.clone()))

        val (card1, card2) = (deck1.dequeue(), deck2.dequeue())
        val player1Wins = if (deck1.length < card1 || deck2.length < card2) {
          card1 > card2
        } else {
          playRecursiveCombatGame(deck1.take(card1), deck2.take(card2))._1
        }

        if (player1Wins) {
          deck1.enqueue(card1, card2)
        } else {
          deck2.enqueue(card2, card1)
        }
      }
    }

    (deck1.nonEmpty, deck1.enqueueAll(deck2))
  }

  def score(deck: collection.Seq[Int]): Int = deck.reverse.zipWithIndex.map { case (card, index) => (index + 1) * card }.sum

  def main(args: Array[String]): Unit = {

    val (deck1, deck2) = getDecks

    // Part 1
    val winningDeckPart1 = playCombat(deck1, deck2)
    println(s"Part 1: winning score is ${score(winningDeckPart1)}")

    // Part 2
    val (_, winningDeckPart2) = playRecursiveCombatGame(mutable.Queue().enqueueAll(deck1), mutable.Queue().enqueueAll(deck2))
    println(s"Part 2: winning score is ${score(winningDeckPart2)}")
  }
}
