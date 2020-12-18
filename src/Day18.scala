import scala.collection.mutable.ListBuffer

object Day18 {

  def main(args: Array[String]): Unit = {

    // Evaluate them with + and = having the same precedence
    val expressions1 = Utils.getInputLines.map(line => tokenize(line))
    val precedences1 = (token: String) => token match {
      case "+" => 1L
      case "*" => 1L
      case _ => 0L
    }
    val sum1 = expressions1.map(evaluate(_, precedences1)).sum
    println(s"Part 1: Sum is $sum1")

    // Evaluate them with + having higher precedence than *
    val expressions2 = Utils.getInputLines.map(line => tokenize(line))
    val precedences2 = (token: String) => token match {
      case "+" => 2L
      case "*" => 1L
      case _ => 0L
    }
    val sum2 = expressions2.map(evaluate(_, precedences2)).sum
    println(s"Part 2: Sum is $sum2")
  }

  def tokenize(expression: String): ListBuffer[String] = {
    ListBuffer.from(
      expression
        .replace("(", "( ")
        .replace(")", " )")
        .split(" ")
        .filter(t => t.nonEmpty)
    )
  }

  type PrecedenceTable = String => Long
  type PrefixEvaluator = (ListBuffer[String], PrecedenceTable) => Long
  type InfixEvaluator = (Long, ListBuffer[String], PrecedenceTable) => Long

  def getInfixEvaluator(token: String): InfixEvaluator = token match {
    case "+" => evaluatePlus
    case "*" => evaluateTimes
  }

  def getPrefixEvaluator(token: String): PrefixEvaluator = token match {
    case "(" => evaluateLParen
    case _ => evaluateNumber
  }

  def evaluateLParen(tokens: ListBuffer[String], precedences: PrecedenceTable): Long = {
    tokens.remove(0) // remove ')'
    val expr = evaluate(tokens, precedences)
    tokens.remove(0) // remove ')'
    expr
  }

  def evaluateNumber(tokens: ListBuffer[String], precedences: PrecedenceTable): Long =
    tokens.remove(0).toLong

  def evaluatePlus(left: Long, tokens: ListBuffer[String], precedences: PrecedenceTable): Long =
    left + evaluate(tokens, precedences, precedences(tokens.remove(0)))

  def evaluateTimes(left: Long, tokens: ListBuffer[String], precedences: PrecedenceTable): Long =
    left * evaluate(tokens, precedences, precedences(tokens.remove(0)))

  def evaluate(tokens: ListBuffer[String], precedences: PrecedenceTable, precedence: Long = 0): Long = {
    if (tokens.isEmpty) return 0L

    var left = getPrefixEvaluator(tokens.head)(tokens, precedences)

    while (tokens.nonEmpty && precedence < precedences(tokens.head)) {
      val infix = getInfixEvaluator(tokens.head)
      left = infix(left, tokens, precedences)
    }

    left
  }

}
