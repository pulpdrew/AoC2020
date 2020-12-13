object Day13 {

  def main(args: Array[String]): Unit = {

    // Part 1
    val readyTime = Utils.getInputLines.head.toLong
    val busIds = Utils.getInputLines.tail.head.split(',').filter(_ != "x").map(_.toLong)
    val earliestBus = busIds.map(id => (id, waitTime(id, readyTime))).minBy(t => t._2)
    println(s"Part 1: ${earliestBus._1 * earliestBus._2}")

    // Part 2

    // (id, time) pairs
    val schedule = Utils.getInputLines
      .tail
      .head
      .split(',')
      .zipWithIndex
      .filter(_._1 != "x")
      .map(p => (p._1.toLong, p._2.toLong))

    // Use Chinese Remainder Theorem to solve system of modulo congruences

    val N = schedule.map(_._1).product

    val t = schedule.map {
      case (id, time) =>

        // Bus arriving at t + time yields congruence
        // t = (id - time) (mod id)

        val a = (id - time) % id
        val y = N / id
        val z = inverseMod(y, id)

        y * z * a
    }.sum % N

    println(s"Part 2: t = $t")
  }

  private def inverseMod(a: Long, m: Long): Long = {
    val (x, _, _) = gcdExtended(a, m)
    (x + m) % m
  }

  // Returns (x, y, g) where ax + by = gcd(a, b) and g = gcd(a, b)
  private def gcdExtended(a: Long, b: Long): (Long, Long, Long) = {
    var rOld = a
    var r = b
    var sOld = 1L
    var s = 0L
    var tOld = 0L
    var t = 1L

    while (r != 0) {
      val quotient = rOld / r

      val rTemp = rOld - quotient * r
      rOld  = r
      r = rTemp

      val sTemp = sOld - quotient * s
      sOld = s
      s = sTemp

      val tTemp = tOld - quotient * t
      tOld = t
      t = tTemp
    }

    (sOld, tOld, rOld)
  }

  private def waitTime(id: Long, readyTime: Long): Long = {
    id - readyTime % id
  }
}
