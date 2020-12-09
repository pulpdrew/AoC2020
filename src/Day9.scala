object Day9 {

  def main(args: Array[String]): Unit = {

    def preambleLength = 25
    val stream = Utils.getInputLines.map(_.toLong)
    val invalidIdx = (preambleLength until stream.length).find(idx => !isValid(idx, stream, preambleLength)).getOrElse(0)
    val invalid = stream(invalidIdx)
    println(s"Part 1: Invalid number is $invalid")

    val sum = findContiguousSum(invalid, stream)
    println(s"Part 2: ${sum.min + sum.max}")
  }

  def findContiguousSum(target: Long, numbers: List[Long]): List[Long] = {
    var start = 0
    var end = 1
    var sum: Long = numbers.slice(0, 2).sum

    while (sum != target) {
      if (sum > target) {
        sum -= numbers(start)
        start += 1
      } else {
        end += 1
        sum += numbers(end)
      }
    }

    numbers.slice(start, end + 1)
  }

  def isValid(idx: Int, stream: List[Long], preambleLength: Int): Boolean = {
    isSum(stream(idx.toInt), stream.slice(idx - preambleLength, idx).toSet)
  }

  def isSum(n: Long, numbers: Set[Long]): Boolean = {
    numbers.exists(first => n - first != first && numbers.contains(n - first))
  }

}
