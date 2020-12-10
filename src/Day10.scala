import scala.collection.mutable

object Day10 {

  def main(args: Array[String]): Unit = {
    val adapters = Utils.getInputLines.map(_.toInt).sorted
    val adaptersWithEndJoltages = 0 :: (adapters :+ (adapters.max + 3))

    // Part 1
    val differences = adaptersWithEndJoltages.foldLeft((Map[Int, Int](), 0)) {
      case ((map, prev), cur) => (map + (cur - prev -> (map.getOrElse(cur - prev, 0) + 1)), cur)
    }._1
    println(s"Part 1: ${differences.getOrElse(1, 0) * differences.getOrElse(3, 0)}")

    // Part 2
    val ars = arrangements(adaptersWithEndJoltages)
    println(s"Part 2: $ars arrangements")
  }

  def arrangements(adapters: List[Int], cache: mutable.Map[Int, Long] = mutable.Map()): Long = {
    if (adapters.length <= 1) {
      1
    } else {
      cache.getOrElseUpdate(adapters.head, {
        adapters.zipWithIndex.filter {
          case (adapter, _) => canConnect(adapter, adapters.head)
        }.map {
          case (_, index) => arrangements(adapters.slice(index, adapters.length), cache)
        }.sum
      })
    }
  }

  private def canConnect(a: Int, b: Int): Boolean = {
    val maxDiff = 3
    (a - b).abs <= maxDiff && a != b
  }
}
