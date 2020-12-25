object Day25 {

  def main(args: Array[String]): Unit = {
    val key1 = Utils.getInputLines.head.toInt
    val key2 = Utils.getInputLines(1).toInt
    println(s"Part 1: ${transform(key1, getLoopSize(key2))}")
  }

  def transform(subject: Int, loops: Int): Long =
    (0 until loops).foldLeft(1L)((value, _) => transformOnce(value, subject))

  def transformOnce(value: Long, subject: Long): Long = value * subject % 20201227

  def getLoopSize(key: Long): Int = {
    var (value, loopCount) = (1L, 0)
    while (value != key) {
      value = transformOnce(value, 7)
      loopCount += 1
    }

    loopCount
  }
}
