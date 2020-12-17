object Day17 {

  type Coord3 = (Int, Int, Int)
  type Space3 = Map[Coord3, Boolean]

  type Coord4 = (Int, Int, Int, Int)
  type Space4 = Map[Coord4, Boolean]

  def main(args: Array[String]): Unit = {

    // Part 1
    var space3: Space3 = Map[(Int, Int, Int), Boolean]()
    Utils.getInputLines.zipWithIndex.foreach {
      case (line, y) => line.zipWithIndex.foreach {
        case (c, x) => space3 = space3.updated((x, y, 0), c == '#')
      }
    }

    for (_ <- 1 to 6) {
      space3 = step3(space3)
    }

    val activeCount1 = space3.count(e => e._2)
    println(s"Part 1: $activeCount1 active cells")

    // Part 2
    var space4: Space4 = Map[(Int, Int, Int, Int), Boolean]()
    Utils.getInputLines.zipWithIndex.foreach {
      case (line, y) => line.zipWithIndex.foreach {
        case (c, x) => space4 = space4.updated((x, y, 0, 0), c == '#')
      }
    }

    for (_ <- 1 to 6) {
      space4 = step4(space4)
    }

    val activeCount2 = space4.count(e => e._2)
    println(s"Part 2: $activeCount2 active cells")
  }

  private def step3(space: Space3): Space3 = {
    val newSpace = collection.mutable.Map[(Int, Int, Int), Boolean]()

    val (xs, ys, zs) = coords(space)
    for (x <- xs.min - 1 to xs.max + 1) {
      for (y <- ys.min - 1 to ys.max + 1) {
        for (z <- zs.min - 1 to zs.max + 1) {

          // The old value of the grid cell
          val current = space.getOrElse((x, y, z), false)

          // Count the neighbors
          val adjCount = countNeighbors(space, (x, y, z))

          // The new value of the grid cell
          if (adjCount == 3 || (current && adjCount == 2)) {
            newSpace((x, y, z)) = true
          }
        }
      }
    }

    newSpace.toMap
  }

  private def step4(space: Space4): Space4 = {

    val newSpace = collection.mutable.Map[(Int, Int, Int, Int), Boolean]()

    val (xs, ys, zs, ws) = coords(space)
    for (x <- xs.min - 1 to xs.max + 1) {
      for (y <- ys.min - 1 to ys.max + 1) {
        for (z <- zs.min - 1 to zs.max + 1) {
          for (w <- ws.min - 1 to ws.max + 1) {

            // The old value of the grid cell
            val current = space.getOrElse((x, y, z, w), false)

            // Count the neighbors
            val adjCount = countNeighbors(space, (x, y, z, w))

            // The new value of the grid cell
            if (adjCount == 3 || (current && adjCount == 2)) {
              newSpace((x, y, z, w)) = true
            }
          }
        }
      }
    }

    newSpace.toMap
  }

  private def countNeighbors(space: Space3, coord: Coord3): Int = {
    var adjCount = 0

    for (adjX <- coord._1 - 1 to coord._1 + 1;
         adjY <- coord._2 - 1 to coord._2 + 1;
         adjZ <- coord._3 - 1 to coord._3 + 1
         if coord != (adjX, adjY, adjZ)) {

      if (space.getOrElse((adjX, adjY, adjZ), false)) {
        adjCount += 1
      }
    }

    adjCount
  }

  private def countNeighbors(space: Space4, coord: Coord4): Int = {
    var adjCount = 0

    for (adjX <- coord._1 - 1 to coord._1 + 1;
         adjY <- coord._2 - 1 to coord._2 + 1;
         adjZ <- coord._3 - 1 to coord._3 + 1;
         adjW <- coord._4 - 1 to coord._4 + 1
         if coord != (adjX, adjY, adjZ, adjW)) {

      if (space.getOrElse((adjX, adjY, adjZ, adjW), false)) {
        adjCount += 1
      }
    }

    adjCount
  }

  private def coords(space: Space3): (List[Int], List[Int], List[Int]) = {
    val xs = space.filter(e => e._2).map(_._1._1).toList
    val ys = space.filter(e => e._2).map(_._1._2).toList
    val zs = space.filter(e => e._2).map(_._1._3).toList

    (xs, ys, zs)
  }

  private def coords(space: Space4): (List[Int], List[Int], List[Int], List[Int]) = {
    val xs = space.filter(e => e._2).map(_._1._1).toList
    val ys = space.filter(e => e._2).map(_._1._2).toList
    val zs = space.filter(e => e._2).map(_._1._3).toList
    val ws = space.filter(e => e._2).map(_._1._4).toList

    (xs, ys, zs, ws)
  }

}
