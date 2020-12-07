object Day6 {

  def main(args: Array[String]): Unit = {
    val groupPattern = "(\\w\n?)+".r
    val groups = groupPattern.findAllMatchIn(Utils.getInputString)
      .map(g => g.matched.split("\n")
        .map(person => person.chars().toArray.toSet)
        .toList
      )
      .toList

    val p1Sum = groups
      .map(g => g.reduceLeft((acc, next) => acc.union(next)))
      .map(g => g.size)
      .sum

    val p2Sum = groups
      .map(g => g.reduceLeft((acc, next) => acc.intersect(next)))
      .map(g => g.size)
      .sum

    println(s"Part 1: $p1Sum. Part 2: $p2Sum")
  }
}
