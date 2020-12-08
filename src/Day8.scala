object Day8 {

  trait Operation

  case class Acc(arg: Int) extends Operation

  case class Nop(arg: Int) extends Operation

  case class Jmp(arg: Int) extends Operation

  case class ConsoleState(acc: Int, pc: Int)

  def execute(program: Array[Operation], state: ConsoleState): ConsoleState = {
    program(state.pc) match {
      case Jmp(arg) => ConsoleState(state.acc, state.pc + arg)
      case Nop(arg) => ConsoleState(state.acc, state.pc + 1)
      case Acc(arg) => ConsoleState(state.acc + arg, state.pc + 1)
    }
  }

  def readProgram(): Array[Operation] = {
    Utils.getInputLines
      .map(_.split(" "))
      .map(parts => (parts(0), parts(1).toInt))
      .map(parts => parts._1 match {
        case "nop" => Nop(parts._2)
        case "acc" => Acc(parts._2)
        case "jmp" => Jmp(parts._2)
      })
      .toArray
  }

  def run(program: Array[Operation]): ConsoleState = {
    val executedAddresses = collection.mutable.Set[Int]()
    var state = ConsoleState(0, 0)

    while (!executedAddresses.contains(state.pc) && state.pc < program.length && state.pc >= 0) {
      executedAddresses.add(state.pc)
      state = execute(program, state)
    }

    state
  }

  def main(args: Array[String]): Unit = {

    val program = readProgram()

    // Part 1
    val finalState = run(program)
    println(s"Executed address at ${finalState.pc} twice. Acc = ${finalState.acc}")

    // Part 2
    val canFlip: Operation => Boolean = {
      case Jmp(arg) => true
      case Nop(arg) => true
      case _ => false
    }

    val flipOp: Operation => Operation = {
      case Jmp(arg) => Nop(arg)
      case Nop(arg) => Jmp(arg)
      case op => op
    }

    for (i <- program.indices if canFlip(program(i))) {
      val candidateProgram = program.updated(i, flipOp(program(i)))
      val endState = run(candidateProgram)
      if (endState.pc == program.length) {
        println(s"Found Terminating program by flipping instruction at $i. Acc = ${endState.acc}")
      }
    }
  }

}

