import scala.collection.mutable

object Day15 {
  def main(args: Array[String]): Unit = {
    val startingNumbers = Utils.getInputLines.mkString("").split(",").map(_.toLong).toList

    println(s"Part 1: 2020th number is ${getNumber(2020, startingNumbers)}")
    println(s"Part 2: 30,000,000th number is ${getNumber(30_000_000, startingNumbers)}")
  }

  def getNumber(time: Int, startingNumbers: List[Long]): Long = {

    // A Map from number -> (t1, t2) where t2 is the last time the number
    // was spoken, and t1 is the second to last time that the number was spoken
    val history = mutable.Map[Long, (Int, Int)]()

    // Populate the history with the starting numbers
    startingNumbers.zipWithIndex.foreach {
      case (n, index) => history(n) = (index + 1, index + 1)
    }

    // the (t1, t2) entry for the number most recently spoken
    var prevs = history(startingNumbers.last)

    // the next number in the sequence
    var next = 0L

    for (t <- (startingNumbers.length + 1) to time) {

      // Calculate the next number using the times the previous number was spoken
      next = prevs._2 - prevs._1

      // Calculate (t1, t2) for the next number
      prevs = history.get(next) match {
        case Some((_, t2)) => (t2, t)
        case None => (t, t)
      }

      // Update the history with the new (t1, t2) for the next number
      history(next) = prevs
    }

    next
  }

}
