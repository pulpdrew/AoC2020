object Day4 {

  def main(args: Array[String]) = {
    println(s"Part 1: ${passports.count(_.isValid1)}")
    println(s"Part 2: ${passports.count(_.isValid2)}")
  }

  private def passports: List[Passport] = {
    val passportPattern = "(\\S+:\\S+\\s)+".r
    val fieldPattern = "(\\S+):(\\S+)".r

    passportPattern
      .findAllMatchIn(Utils.getInputString)
      .map(
        m => fieldPattern.findAllMatchIn(m.matched).map(m => (m.group(1), m.group(2))).toMap
      )
      .map(new Passport(_))
      .toList
  }
}
class Passport(val fields: Map[String, String]) {
  def isValid1: Boolean = {
    List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").forall(fields.contains)
  }

  def isValid2: Boolean = {
    fields.getOrElse("byr", "").matches("\\d\\d\\d\\d") &&
      isNumberBetween(fields.getOrElse("byr", ""), 1920, 2020) &&
      fields.getOrElse("iyr", "").matches("\\d\\d\\d\\d") &&
      isNumberBetween(fields.getOrElse("iyr", ""), 2010, 2020) &&
      fields.getOrElse("eyr", "").matches("\\d\\d\\d\\d") &&
      isNumberBetween(fields.getOrElse("eyr", ""), 2020, 2030) &&
      (
        (fields.getOrElse("hgt", "").matches("\\d\\d\\dcm") &&
          isNumberBetween(fields.getOrElse("hgt", "").split('c')(0), 150, 193)) ||
          (fields.getOrElse("hgt", "").matches("\\d\\din") &&
            isNumberBetween(fields.getOrElse("hgt", "").split('i')(0), 59, 76))
        ) &&
      fields.getOrElse("hcl", "").matches("#[0-9a-f]{6}") &&
      List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(fields.getOrElse("ecl", "")) &&
      fields.getOrElse("pid", "").matches("\\d{9}")
  }

  private def isNumberBetween(s: String, min: Int, max: Int): Boolean = {
    try {
      s.toInt <= max && s.toInt >= min
    } catch {
      case e: NumberFormatException => false
    }
  }
}