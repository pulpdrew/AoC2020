import scala.annotation.tailrec

object Day24 {

  def main(args: Array[String]): Unit = {

    // Part 1
    val renovationList = Utils.getInputLines.map(toDirections)
    val blackTiles = getBlackTiles(renovationList)
    println(s"Part 1: ${blackTiles.size} black tiles after renovation")

    // Part 2
    var blackSet = blackTiles
    for (_ <- 1 to 100) {
      blackSet = step(blackSet)
    }
    println(s"Part 2: ${blackSet.size} black tiles after 100 days.")
  }

  def step(blackTiles: Set[(Double, Double)]): Set[(Double, Double)] = {
    val (minX, maxX, minY, maxY) = (
      blackTiles.minBy(_._1)._1,
      blackTiles.maxBy(_._1)._1,
      blackTiles.minBy(_._2)._2,
      blackTiles.maxBy(_._2)._2,
    )

    Utils.cartesian(
      (minX - 2).toInt to (maxX + 2).toInt,
      (minY - 2).toInt to (maxY + 2).toInt
    )
      .map {
        case (x, y) => if (y % 2 != 0) {
          (x + .5, y.toDouble)
        } else {
          (x.toDouble, y.toDouble)
        }
      }
      .filter(willBeBlack(_, blackTiles))
      .toSet
  }

  def willBeBlack(tile: (Double, Double), blackTiles: Set[(Double, Double)]): Boolean = {
    val blackNeighbors = blackNeighborCount(tile, blackTiles)
    blackNeighbors == 2 || (blackTiles.contains(tile) && blackNeighbors == 1)
  }

  def blackNeighborCount(tile: (Double, Double), blackTiles: Set[(Double, Double)]): Int =
    List(
      (tile._1 - 1, tile._2),
      (tile._1 + 1, tile._2),
      (tile._1 - .5, tile._2 - 1.0),
      (tile._1 + .5, tile._2 - 1.0),
      (tile._1 - .5, tile._2 + 1.0),
      (tile._1 + .5, tile._2 + 1.0),
    ).count(blackTiles.contains)

  def getBlackTiles(renovationList: List[List[String]]): Set[(Double, Double)] =
    renovationList.map(navigate).foldLeft(Set[(Double, Double)]()) {
      case (blackTiles, nextTile) => if (blackTiles.contains(nextTile))
        blackTiles - nextTile
      else
        blackTiles + nextTile
    }

  def toDirections(str: String): List[String] = {

    @tailrec
    def toDirectionsHelper(remaining: String, prefix: List[String]): List[String] = {
      if (remaining.isEmpty) {
        prefix.reverse
      } else {
        if (remaining.startsWith("se")) {
          toDirectionsHelper(remaining.drop(2), "se" :: prefix)
        } else if (remaining.startsWith("sw")) {
          toDirectionsHelper(remaining.drop(2), "sw" :: prefix)
        } else if (remaining.startsWith("ne")) {
          toDirectionsHelper(remaining.drop(2), "ne" :: prefix)
        } else if (remaining.startsWith("nw")) {
          toDirectionsHelper(remaining.drop(2), "nw" :: prefix)
        } else if (remaining.startsWith("w")) {
          toDirectionsHelper(remaining.drop(1), "w" :: prefix)
        } else {
          toDirectionsHelper(remaining.drop(1), "e" :: prefix)
        }
      }
    }

    toDirectionsHelper(str, List())
  }

  def navigate(directions: List[String]): (Double, Double) = {
    if (directions.isEmpty) {
      (0, 0)
    } else {
      val offset: (Double, Double) = directions.head match {
        case "w" => (-1, 0.0)
        case "e" => (1, 0.0)
        case "sw" => (-.5, -1.0)
        case "se" => (.5, -1.0)
        case "nw" => (-.5, 1.0)
        case "ne" => (.5, 1.0)
      }

      val remainingMovement = navigate(directions.tail)
      (remainingMovement._1 + offset._1, remainingMovement._2 + offset._2)
    }
  }
}
