import scala.annotation.tailrec

object Day13 {

  def main(args: Array[String]): Unit = {

    // Part 1
    val readyTime = Utils.getInputLines.head.toLong
    val busIds = Utils.getInputLines.tail.head.split(',').filter(_ != "x").map(_.toLong)
    val earliestBus = busIds.map(id => (id, waitTime(id, readyTime))).minBy(t => t._2)
    println(s"Part 1: ${earliestBus._1 * earliestBus._2}")

    // Part 2

    // The desired arrival schedule as pairs (<bus id>, <time offset>)
    val schedule = Utils.getInputLines(1)
      .split(',')
      .zipWithIndex
      .filter(_._1 != "x")
      .map { case (id, idx) => (id.toLong, idx.toLong) }
      .toList

    // The corresponding system of congruences x = a (mod m)...
    // represented as tuples (a, m)
    val system = schedule.map {
      case (id, time) => ((id - time) % id, id)
    }

    // Use the Chinese Remainder Theorem to solve for the time
    val t = chineseRemainder(system)
    println(s"Part 2: t = $t")
  }

  // Finds the first solution x to the given system of congruences,
  // where each tuple (a, m) corresponds to x = a (mod m)
  private def chineseRemainder(system: List[(Long, Long)]): Long = {
    val N = system.map(_._2).product
    system.map {
      case (a, m) =>
        val y = N / m
        val z = inverseMod(y, m)
        a * y * z
    }.sum % N
  }

  // Returns the solution x to
  // x = a (mod m)
  private def inverseMod(a: Long, m: Long): Long = {
    val (x, _, _) = gcd(a, m)
    (x + m) % m
  }

  // Extended Euclid Algorithm, finds (x, y, g) for
  // ax + by = gcd(a, b) and g = gcd(a, b)
  private def gcd(a: Long, b: Long): (Long, Long, Long) = {
    if (a == 0) {
      (0, 1, b)
    } else {
      val (x, y, g) = gcd(b % a, a)
      (y - (b/a) * x, x, g)
    }
  }

  private def waitTime(id: Long, readyTime: Long): Long = {
    id - readyTime % id
  }
}
