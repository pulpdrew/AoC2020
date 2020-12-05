object Day5 {
  def main(args: Array[String]) = {
    val seats = Utils.getInputLines
      .map(
        line => line
          .replace('B', '1')
          .replace('F', '0')
          .replace('R', '1')
          .replace('L', '0')
      ).map(Integer.parseInt(_, 2))

    val max = seats.max
    val missing = Range(0, max).inclusive.filter(seat => !seats.contains(seat) && seats.contains(seat - 1) && seats.contains(seat + 1)).head

    println(s"Max Seat ID: $max. Missing Seat ID: $missing")
  }
}
