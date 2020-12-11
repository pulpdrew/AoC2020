import scala.collection.mutable.ListBuffer

object Day11 {

  def main(args: Array[String]): Unit = {

    val initialFloorPlan = toFloorPlan(Utils.getInputLines)

    // Part 1
    var current = initialFloorPlan
    var next = current.step(current.getAdjacent, 4)
    while (!next.equals(current)) {
      current = next
      next = current.step(current.getAdjacent, 4)
    }

    val occupiedP1 = next.spaces.count(_ == Occupied)
    println(s"Part 1: $occupiedP1 Occupied Seats After Convergence")

    // Part 2
    current = initialFloorPlan
    next = current.step(current.getAllVisible, 5)
    while (next != current) {
      current = next
      next = current.step(current.getAllVisible, 5)
    }

    val occupiedP2 = next.spaces.count(_ == Occupied)
    println(s"Part 2: $occupiedP2 Occupied Seats After Convergence")
  }

  class FloorPlan(val spaces: Array[Space], val width: Int) {
    def step(neighborFunction: (Int, Int) => List[Space], occupiedThreshold: Int): FloorPlan = {
      val newSpaces = new Array[Space](this.spaces.length)

      for (x <- 0 until width) {
        for (y <- 0 until spaces.length / width) {
          val current = this.get(x, y).get
          val neighbors = neighborFunction(x, y)

          newSpaces(x + y * width) = current match {
            case Empty => if (!neighbors.contains(Occupied)) {
              Occupied
            } else {
              Empty
            }
            case Occupied => if (neighbors.count(_ == Occupied) >= occupiedThreshold) {
              Empty
            } else {
              Occupied
            }
            case Floor => Floor
          }
        }
      }

      new FloorPlan(newSpaces, this.width)
    }

    def getAdjacent(x: Int, y: Int): List[Space] = {
      List(
        this.get(x + 1, y + 0),
        this.get(x + 1, y + 1),
        this.get(x + 0, y + 1),
        this.get(x - 1, y + 1),
        this.get(x - 1, y + 0),
        this.get(x - 1, y - 1),
        this.get(x + 0, y - 1),
        this.get(x + 1, y - 1)
      ).filter(_.isDefined).map(_.get)
    }

    def getAllVisible(x: Int, y: Int): List[Space] = {
      List(
        this.getVisible(x, y, +1, +0),
        this.getVisible(x, y, +1, +1),
        this.getVisible(x, y, +0, +1),
        this.getVisible(x, y, -1, +1),
        this.getVisible(x, y, -1, +0),
        this.getVisible(x, y, -1, -1),
        this.getVisible(x, y, +0, -1),
        this.getVisible(x, y, +1, -1)
      ).filter(_.isDefined).map(_.get)
    }

    private def getVisible(fromX: Int, fromY: Int, offsetX: Int, offsetY: Int): Option[Space] = {
      var x = fromX + offsetX
      var y = fromY + offsetY
      var seat = this.get(x, y)
      while (seat.isDefined && seat.get == Floor) {
        x += offsetX
        y += offsetY
        seat = this.get(x, y)
      }

      seat
    }

    def get(x: Int, y: Int): Option[Space] = {
      if (x >= 0 && y >= 0 && y < this.spaces.length / width && x < this.width) {
        Some(this.spaces(x + y * this.width))
      } else {
        None
      }
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case f: FloorPlan => f.spaces.sameElements(this.spaces) && this.width == f.width
        case _ => false
      }
    }

    override def toString: String = {
      val string = new StringBuilder()

      for (y <- 0 until this.spaces.length / width) {
        for (x <- 0 until this.width) {
          string.append(this.get(x, y).get match {
            case Floor => "."
            case Empty => "L"
            case Occupied => "#"
          })
        }
        string.append('\n')
      }

      string.toString()
    }
  }

  private def toFloorPlan(lines: List[String]): FloorPlan = {
    val width = lines.head.length;
    val spaces = lines.mkString("").map(toSpace).toArray

    new FloorPlan(spaces, width)
  }

  private def toSpace(c: Char): Space = {
    c match {
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor
    }
  }

  sealed trait Space

  case object Occupied extends Space

  case object Empty extends Space

  case object Floor extends Space

}
