object Day3 {

  def main(args: Array[String]): Unit = {
    val map = Utils.getInputLines

    val p1Count = countTrees(map, (3, 1))
    println(s"Part 1: $p1Count trees")

    val p2Count = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(countTrees(map, _))
      .foldLeft(BigInt(1)) { case (product, next) => product * next}
    println(s"Part 2: $p2Count trees")
  }

  private def countTrees(map: List[String], slope: (Int, Int)) = {
    map
      .zipWithIndex
      .filter(_._2 % slope._2 == 0)
      .map {
        case (line, index) => line.charAt((index / slope._2 * slope._1) % line.length)
      }
      .count(_ == '#')
  }
}
