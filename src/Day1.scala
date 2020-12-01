import Utils.{cartesian, countItems, getInputLines}

object Day1 {
  def main(args: Array[String]): Unit = {
    val entries = getInputLines.map(_.toInt)
    val counts = countItems(entries)
    val target = 2020

    // Part 1
    val p1First = entries
      .filter(hasPair(counts, target, _))
      .head
    val p1Second = target - p1First
    println(s"$p1First * $p1Second = ${p1First * p1Second}")

    // Part 2
    val (p2First, p2Second) = cartesian(entries, entries)
      .filter(pair => hasTriple(counts, target, pair._1, pair._2))
      .head
    val p2Third = target - p2First - p2Second
    println(s"$p2First * $p2Second * $p2Third = ${p2First * p2Second * p2Third}")
  }

  private def hasTriple(counts: Map[Int, Int], targetSum: Int, first: Int, second: Int) = {
    val third = targetSum - first - second
    val numberCounts = countItems(List(first, second, third))
    numberCounts.forall(entry => counts.getOrElse(entry._1, 0) >= entry._2)
  }

  private def hasPair(counts: Map[Int, Int], targetSum: Int, first: Int) = {
    if (targetSum - first == first) {
      counts.getOrElse(first, 0) >= 2
    } else {
      counts.getOrElse(targetSum - first, 0) >= 1
    }
  }
}
