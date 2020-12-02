object Utils {
  /**
   * Count the number of occurrences of each unique item in items
   */
  def countItems[T](items: Iterable[T]): Map[T, Int] = {
    items.foldLeft(collection.mutable.Map[T, Int]())(addToCount).toMap.withDefaultValue(0)
  }

  /**
   * Record an occurrence of the given item in the map of items -> occurrence count
   */
  private def addToCount[T](counts: collection.mutable.Map[T, Int], item: T) = {
    counts(item) = counts.getOrElse(item, 0) + 1
    counts
  }

  /**
   * Read data.txt as a string
   */
  def getInputString: String = {
    getInputLines.mkString("\n")
  }

  /**
   * Read and return the list of lines from data.txt
   */
  def getInputLines: List[String] = {
    val fileName = "data.txt"
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  /**
   * Returns the cartesian product of the two given iterables
   */
  def cartesian[A, B](a: Iterable[A], b: Iterable[B]): Iterable[(A, B)] = {
    a.flatMap(first => b.map(second => (first, second)))
  }
}
