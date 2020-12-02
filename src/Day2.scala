object Day2 {
  def main(args: Array[String]): Unit = {
    val entries = Utils
      .getInputLines
      .map(_.split(":"))
      .map(parts => (new Policy(parts(0)), parts(1).trim()))

    val p1Valid = entries.filter(pair => isValid1(pair._1, pair._2))
    val p2Valid = entries.filter(pair => isValid2(pair._1, pair._2))

    println(s"There are ${p1Valid.length} valid passwords for part 1")
    println(s"There are ${p2Valid.length} valid passwords for part 2")
  }

  class Policy(description: String) {
    private val pattern = "(\\d+)-(\\d+) (.)".r
    private val matches = pattern.findFirstMatchIn(description)

    val (min, max, letter) = matches match {
      case Some(m) => (m.group(1).toInt, m.group(2).toInt, m.group(3).charAt(0))
      case None => throw new Exception(s"Failed to extract policy from $description")
    }
  }

  private def isValid1(policy: Policy, password: String): Boolean = {
    val count = Utils.countItems(password).getOrElse(policy.letter, 0)
    count <= policy.max && count >= policy.min
  }

  private def isValid2(policy: Policy, password: String): Boolean = {
    password.charAt(policy.min - 1) != password.charAt(policy.max - 1) && (password.charAt(policy.min - 1) == policy.letter || password.charAt(policy.max - 1) == policy.letter)
  }
}


