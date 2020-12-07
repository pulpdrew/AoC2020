import scala.util.matching.Regex

object Day7 {

  def main(args: Array[String]): Unit = {
    val rules = getRules

    val p1 = rules.keys.count(color => canHoldShinyGold(color, rules))
    println(s"Part 1: $p1 bags can hold shiny gold bags")

    val p2 = countOfOtherBags("shiny gold", rules)
    println(s"Part 2: shiny gold bags hold $p2 other bags")
  }

  def getRules: Map[String, List[Content]] = {
    val rulePattern: Regex = "(\\d+)? ?([a-z][a-z\\s]*?) bag".r

    Utils.getInputLines
      .map(line => rulePattern.findAllMatchIn(line).map(m => (m.group(2), m.group(1))).toList)
      .map(colors => {
        if (colors.last._1.contains("contain no other")) {
          (colors.head._1, List())
        } else {
          (colors.head._1, colors.tail)
        }
      })
      .map(rule => (rule._1, rule._2.map(contents => Content(contents._1, contents._2.toInt))))
      .toMap
  }

  def countOfOtherBags(start: String, rules: Map[String, List[Content]]): Int = {
    if (rules(start).isEmpty) {
      0
    } else {
      rules(start).map(c => c.quantity + c.quantity * countOfOtherBags(c.color, rules)).sum
    }
  }

  def canHoldShinyGold(start: String, rules: Map[String, List[Content]]): Boolean = {
    if (rules(start).isEmpty) {
      false
    } else {
      rules(start).exists(content => content.color == "shiny gold") ||
        rules(start).exists(content => canHoldShinyGold(content.color, rules))
    }
  }

  case class Content(color: String, quantity: Int)

}


