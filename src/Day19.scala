import scala.annotation.tailrec
import scala.util.matching.Regex

object Day19 {

  def main(args: Array[String]): Unit = {

    val ruleDefinitions = Utils.getInputLines.takeWhile(line => line.nonEmpty).map(line => {
      val parts = line.split(":").map(_.trim)
      (parts(0), parts(1))
    }).toMap

    val messages = Utils.getInputLines.dropWhile(line => line.nonEmpty).tail

    // Part 1
    val rule0Regex = new Regex(formatAsRegex(ruleDefinitions("0"), ruleDefinitions))
    val matchingMessageCount1 = messages.count(rule0Regex.matches(_))
    println(s"Part 1: $matchingMessageCount1 matching messages.")

    // Part 2

    val rule42 = formatAsRegex(ruleDefinitions("42"), ruleDefinitions)
    val rule31 = formatAsRegex(ruleDefinitions("31"), ruleDefinitions)
    val prefix42 = new Regex("^" + rule42)
    val suffix31 = new Regex(rule31 + "$")

    @tailrec
    def matchesRule11(message: String): Boolean = {
      val prefix = prefix42.findFirstMatchIn(message).map(_.matched)
      val suffix = suffix31.findFirstMatchIn(message).map(_.matched)

      (prefix, suffix) match {
        case (Some(prefix), Some(suffix)) =>
          if (prefix.length + suffix.length < message.length) {
            matchesRule11(message.slice(prefix.length, message.length - suffix.length))
          } else {
            prefix.length + suffix.length == message.length
          }
        case _ => false
      }
    }

    def matchesRule0(message: String): Boolean = {
      getPrefixMatches(message, rule42).exists(m => {
        matchesRule11(message.slice(m.length, message.length))
      })
    }

    val matchingMessageCount2 = messages.count(matchesRule0)
    println(s"Part 2: $matchingMessageCount2 matching messages.")
  }

  // Finds prefixes of `message` that match the given regex pattern when it is repeated between 1 and `maxRepetitions` times
  def getPrefixMatches(message: String, pattern: String, maxRepetitions: Int = 5): List[String] = {
    (1 to maxRepetitions).flatMap(repetitions => new Regex(s"^$pattern{$repetitions}").findAllMatchIn(message).map(_.matched)).toList
  }

  // Formats the given rule as a regex pattern, using the provided subRule definitions
  def formatAsRegex(ruleDefinition: String, ruleDefinitions: Map[String, String]): String = {
    if (ruleDefinition.matches("\".+\"")) {
      ruleDefinition.replace("\"", "")
    } else {
      ruleDefinition
        .split("\\|")
        .map(alternation => alternation.trim.split(" ").map(subRule => formatAsRegex(ruleDefinitions(subRule), ruleDefinitions)))
        .map(alternation => alternation.mkString(""))
        .mkString("(", "|", ")")
    }
  }
}
