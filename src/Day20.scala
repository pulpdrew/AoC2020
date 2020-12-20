object Day20 {

  val tileWidth = 10

  case class Tile(id: Int, imageLines: List[List[Char]]) {

    val (left, right, top, bottom, content) = (
      borderToNumber(imageLines.map(l => l.head)),
      borderToNumber(imageLines.map(l => l.last)),
      borderToNumber(imageLines.head),
      borderToNumber(imageLines.last),
      imageLines.slice(1, tileWidth - 1).map(line => line.slice(1, tileWidth - 1))
    )

    def rotated: Tile = Tile(id, imageLines.transpose.map(l => l.reverse))

    def flipped: Tile = Tile(id, imageLines.map(l => l.reverse))

    private def borderToNumber(border: List[Char]): Int =
      Integer.parseInt(border.mkString("").replace("#", "1").replace(".", "0"), 2)
  }

  def getTiles: List[Tile] = {
    var tiles: List[Tile] = List()
    var remainingInput = Utils.getInputLines.map(_.toList)

    while (remainingInput.nonEmpty) {
      val tileInput = remainingInput.take(tileWidth + 1)
      remainingInput = remainingInput.drop(tileWidth + 2)

      val id = tileInput.head.slice(5, 9).mkString.toInt
      tiles = Tile(id, tileInput.slice(1, tileWidth + 1)) :: tiles
    }

    tiles.flatMap(tile => List(
      tile,
      tile.rotated,
      tile.rotated.rotated,
      tile.rotated.rotated.rotated,
      tile.flipped,
      tile.flipped.rotated,
      tile.flipped.rotated.rotated,
      tile.flipped.rotated.rotated.rotated
    ))
  }

  def nextTiles(known: List[Tile], remaining: List[Tile], sideLength: Int): List[Tile] = {
    val left = if (known.length % sideLength == 0) {
      None
    } else {
      Some(known.last.right)
    }

    val top = if (known.length < sideLength) {
      None
    } else {
      Some(known(known.size - sideLength).bottom)
    }

    (left, top) match {
      case (Some(l), Some(t)) => remaining.filter(tile => tile.left == l && tile.top == t)
      case (None, Some(t)) => remaining.filter(tile => tile.top == t)
      case (Some(l), None) => remaining.filter(tile => tile.left == l)
      case (None, None) => remaining
    }
  }

  def solveTiles(known: List[Tile], remaining: List[Tile], sideLength: Int): Option[List[Tile]] = {
    if (remaining.isEmpty) {
      Some(known)
    } else {
      for (next <- nextTiles(known, remaining, sideLength)) {
        val solution = solveTiles(known :+ next, remaining.filter(tile => tile.id != next.id), sideLength)
        if (solution.isDefined) {
          return solution
        }
      }

      None
    }
  }

  def assemble(tiles: List[Tile], sideLength: Int): List[List[Char]] = {
    if (tiles.isEmpty) {
      List()
    } else {
      val (firstRowTiles, rest) = tiles.splitAt(sideLength)
      (0 until tileWidth - 2).map(rowIndex =>
        firstRowTiles
          .map(_.content)
          .flatMap(_ (rowIndex))
      ).toList.concat(assemble(rest, sideLength))
    }
  }

  def find(line: List[Char], pattern: String): Set[Int] =
    (0 until line.length - pattern.length).filter(start => line.mkString.slice(start, start + pattern.length).matches(pattern)).toSet

  def rotated(image: List[List[Char]]): List[List[Char]] =
    image.transpose.map(line => line.reverse)

  def flipped(image: List[List[Char]]): List[List[Char]] = image.map(line => line.reverse)

  def countMonsters(image: List[List[Char]]): Int = {
    (0 until image.length - 3).map(topRowIndex => {
      val topLineIndexes = find(image(topRowIndex), "..................#.")
      val middleLineIndexes = find(image(topRowIndex + 1), "#....##....##....###")
      val bottomLineIndexes = find(image(topRowIndex + 2), ".#..#..#..#..#..#...")

      topLineIndexes.intersect(middleLineIndexes.intersect(bottomLineIndexes)).size
    }).sum
  }

  def roughWaterCount(image: List[List[Char]]): Int = {
    val orientations = List(
      image,
      rotated(image),
      rotated(rotated(image)),
      rotated(rotated(rotated(image))),
      flipped(image),
      flipped(rotated(image)),
      flipped(rotated(rotated(image))),
      flipped(rotated(rotated(rotated(image)))),
    )

    image.flatten.count(c => c == '#') - (orientations.map(image => countMonsters(image)).max * 15)
  }

  def main(args: Array[String]): Unit = {
    val sideLength = 12
    val solution = solveTiles(List(), getTiles, sideLength).get
    val corners = List(solution.head.id, solution.last.id, solution(sideLength - 1).id, solution(solution.size - sideLength).id)

    println(s"Part 1: Product of corners = ${corners.map(id => id.toLong).product}")

    val image = assemble(solution, sideLength)

    println(s"Part 2: Rough water count = ${roughWaterCount(image)}")
  }

}
