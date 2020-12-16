import scala.util.matching.Regex

object Day16 {

  def main(args: Array[String]): Unit = {
    // Part 1
    val ticketErrorRate = findInvalid(getNearbyTickets, getFieldRanges.values.toList).sum
    println(s"Part 1: Error Rate = $ticketErrorRate")

    // Part 2
    val myTicket = getMyTicket
    val fieldOrdering = findOrdering(getValidTickets, getFieldRanges)
    val departureFields = fieldOrdering.zipWithIndex.filter {
      case (name, _) => name.startsWith("departure")
    }.map {
      case (_, idx) =>  myTicket(idx).toLong
    }

    println(s"Part 2: Product of Departure Fields = ${departureFields.product}")
  }

  def findInvalid(tickets: List[List[Int]], ranges: List[(Range, Range)]): List[Int] = {
    tickets.map(findInvalidField(_, ranges)).filter(_.isDefined).map(_.get)
  }

  def findInvalidField(ticket: List[Int], ranges: List[(Range, Range)]): Option[Int] = {
    ticket.find(field => !isValid(field, ranges))
  }

  def isValid(value: Int, ranges: List[(Range, Range)]): Boolean = {
    ranges.exists {
      case (r1, r2) => r1.contains(value) || r2.contains(value)
    }
  }

  def findOrdering(tickets: List[List[Int]], ranges: Map[String, (Range, Range)]): List[String] = {
    var candidates = tickets
      .transpose
      .map(column => findValidRanges(column, ranges))

    val used = collection.mutable.Set[String]()
    val known = new Array[String](ranges.size)

    while (known.contains(null)) {

      // Find columns with exactly one valid field name
      candidates.zipWithIndex.filter {
        case (validFieldNames, _) => validFieldNames.size == 1
      }.foreach {
        case (validFieldNames, idx) =>
          known(idx) = validFieldNames.head
          used += validFieldNames.head
      }

      // Remove any now-used field names, so that they can't be used for other columns
      candidates = candidates.map(s => s.diff(used))
    }

    known.toList
  }

  def removeFromAll(remove: String, lists: List[List[String]]): List[List[String]] = {
    lists.map(list => list.filter(e => e != remove))
  }

  def findValidRanges(column: List[Int], ranges: Map[String, (Range, Range)]): Set[String] = {
    ranges.filter {
      case (_, r) => rangeWorks(column, r)
    }.keys.toSet
  }

  def rangeWorks(column: List[Int], ranges: (Range, Range)): Boolean = {
    column.forall(v => ranges._1.contains(v) || ranges._2.contains(v))
  }

  def getValidTickets: List[List[Int]] = {
    getNearbyTickets.filter(ticket => findInvalidField(ticket, getFieldRanges.values.toList).isEmpty)
  }

  def getMyTicket: List[Int] = {
    val lines = Utils.getInputLines
    val myTicketIdx = lines.indexOf("your ticket:") + 1
    lines(myTicketIdx).split(",").map(_.toInt).toList
  }

  def getNearbyTickets: List[List[Int]] = {
    val lines = Utils.getInputLines
    val nearbyStartIndex = lines.indexOf("nearby tickets:") + 1
    lines.slice(nearbyStartIndex, lines.length).map(_.split(",").map(_.toInt).toList)
  }

  def getFieldRanges: Map[String, (Range, Range)] = {
    val rulePattern: Regex = "([\\w\\s]+): ([\\d]+)-(\\d+) or ([\\d]+)-(\\d+)".r
    Utils.getInputLines.map(rulePattern.findFirstMatchIn(_)).filter(_.isDefined).map(_.get).map(m => {
      val r1 = m.group(2).toInt to m.group(3).toInt
      val r2 = m.group(4).toInt to m.group(5).toInt

      (m.group(1), (r1, r2))
    }).toMap
  }

}
