import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day23 {

  def take(cups: mutable.HashMap[Int, Int], start: Int, count: Int): List[Int] = {
    val list = new ListBuffer[Int]()

    var current = start
    for (_ <- 1 to count) {
      list.append(current)
      current = cups(current)
    }

    list.toList
  }

  def makeMove(current: Int, cups: mutable.HashMap[Int, Int]): Int = {
    val next1 = cups(current)
    val next2 = cups(next1)
    val next3 = cups(next2)
    val next4 = cups(next3)

    val target = List(
      (current + cups.size - 2) % cups.size + 1,
      (current + cups.size - 3) % cups.size + 1,
      (current + cups.size - 4) % cups.size + 1,
      (current + cups.size - 5) % cups.size + 1,
    ).find(t => t != next1 && t != next2 && t != next3).get

    val targetNext = cups(target)
    cups(next3) = targetNext
    cups(target) = next1
    cups(current) = next4

    next4
  }

  def buildNextsMap(list: List[Int]): mutable.HashMap[Int, Int] =
    new mutable.HashMap[Int, Int]().addAll((list :+ list.head).sliding(2).map { case List(n, next) => (n, next) })

  def main(args: Array[String]): Unit = {

    val inputCups = Utils.getInputLines.head.map(_.asDigit).toList

    // Part 1
    val circlePart1 = buildNextsMap(inputCups)
    var current = inputCups.head
    for (_ <- 1 to 100) {
      current = makeMove(current, circlePart1)
    }
    println(s"Part 1: ${take(circlePart1, circlePart1(1), circlePart1.size - 1).mkString}")

    // Part 2
    val circlePart2 = buildNextsMap(inputCups ::: (inputCups.max + 1 to 1000000).toList)

    current = inputCups.head
    for (_ <- 1 to 10000000) {
      current = makeMove(current, circlePart2)
    }
    println(s"Part 2: ${take(circlePart2, circlePart2(1), 2).map(_.toLong).product}")
  }


}
