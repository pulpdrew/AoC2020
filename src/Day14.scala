import scala.util.matching.Regex

object Day14 {

  def main(args: Array[String]): Unit = {

    // Read the instructions
    val maskRegex: Regex = "mask = ([X10]+)".r
    val memRegex: Regex = "mem\\[(\\d+)] = (\\d+)".r
    val instructions = Utils.getInputLines.map {
      case maskRegex(mask) => SetMask(Mask(mask))
      case memRegex(address, value) => SetMem(address.toLong, value.toLong)
    }

    // Part 1
    val initialMask = Mask("X")
    val initialMem = Map[Long, Long]()
    val endMemory1 = instructions.foldLeft((initialMask, initialMem)) {
      case ((_, mem), SetMask(newMask)) => (newMask, mem)
      case ((mask, mem), SetMem(address, value)) => (mask, mem.updated(address, mask.mask(value)))
    }._2
    println(s"Part 1: Sum of Memory Values = ${endMemory1.values.sum}")

    // Part 2
    val endMemory2 = instructions.foldLeft((initialMask, initialMem)) {
      case ((_, mem), SetMask(newMask)) => (newMask, mem)
      case ((mask, mem), SetMem(address, value)) => (mask, mask.getAddresses(address).foldLeft(mem) {
        case (mem, address) => mem.updated(address, value)
      })
    }._2
    println(s"Part 2: Sum of Memory Values = ${endMemory2.values.sum}")

  }

  sealed trait Instruction

  case class SetMask(mask: Mask) extends Instruction

  case class SetMem(address: Long, l: Long) extends Instruction

  case class Mask(source: String) {
    private val ones = java.lang.Long.parseLong(source.replaceAll("X", "0"), 2)
    private val zeroes = java.lang.Long.parseLong(source.replaceAll("X", "1"), 2)

    def mask(to: Long): Long = (to & zeroes) | ones

    def getAddresses(address: Long): List[Long] = {
      source.chars().toArray.reverse.zipWithIndex.map {
        case ('1', _) => List(1L)
        case ('0', pos) => List((address & (1L << pos)) >> pos)
        case ('X', _) => List(0L, 1L)
      }.reverse
        .foldLeft(Set[Long](0)) {
          case (prefixes, nextDigits) => prefixes.flatMap(prefix => nextDigits.map(dig => prefix * 2L + dig))
        }.toList
    }
  }

}
