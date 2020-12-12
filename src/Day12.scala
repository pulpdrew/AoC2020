import Day12.{Instruction, Ship1}

object Day12 {

  def main(args: Array[String]): Unit = {
    val instructions = getInstructions

    val finalShip1 = instructions.foldLeft(Ship1()) {
      case (ship, inst) => ship.move(inst)
    }
    println(s"Part 1: Manhattan Distance is ${finalShip1.x.abs + finalShip1.y.abs}")

    val finalShip2 = instructions.foldLeft(Ship2()) {
      case (ship, inst) => ship.move(inst)
    }
    println(s"Part 2: Manhattan Distance is ${finalShip2.shipX.abs + finalShip2.shipY.abs}")
  }

  case class Ship2(shipX: Int = 0, shipY: Int = 0, wayX: Int = 10, wayY: Int = 1) {
    def move(inst: Instruction): Ship2 = {
      inst match {
        case North(d) => Ship2(shipX, shipY, wayX, wayY + d)
        case South(d) => Ship2(shipX, shipY, wayX, wayY - d)
        case East(d) => Ship2(shipX , shipY, wayX + d, wayY)
        case West(d) => Ship2(shipX, shipY, wayX - d, wayY)
        case Left(a) => a match {
          case 0 =>  Ship2(shipX, shipY, wayX, wayY)
          case 90 =>  Ship2(shipX, shipY, -wayY, wayX)
          case 180 =>  Ship2(shipX, shipY, -wayX, -wayY)
          case 270 =>  Ship2(shipX, shipY, wayY, -wayX)
        }
        case Right(a) => a match {
          case 0 =>  Ship2(shipX, shipY, wayX, wayY)
          case 90 =>  Ship2(shipX, shipY, wayY, -wayX)
          case 180 =>  Ship2(shipX, shipY, -wayX, -wayY)
          case 270 =>  Ship2(shipX, shipY, -wayY, wayX)
        }
        case Forward(d) => Ship2(shipX + wayX * d, shipY + wayY * d, wayX, wayY)
      }
    }
  }

  case class Ship1(x: Int = 0, y: Int = 0, facing: Int = 0) {
    def move(inst: Instruction): Ship1 = {
      inst match {
        case North(d) => Ship1(x, y + d, facing)
        case South(d) => Ship1(x, y - d, facing)
        case East(d) => Ship1(x + d, y, facing)
        case West(d) => Ship1(x - d, y, facing)
        case Left(a) => Ship1(x, y, (facing + a) % 360)
        case Right(a) => Ship1(x, y, (facing - a + 360) % 360)
        case Forward(d) => facing match {
          case 0 => Ship1(x + d, y, facing)
          case 90 => Ship1(x, y + d, facing)
          case 180 => Ship1(x - d, y, facing)
          case 270 => Ship1(x, y - d, facing)
        }
      }
    }
  }

  private def getInstructions: List[Instruction] = {
    Utils.getInputLines.map(toInstruction).toList
  }

  private def toInstruction(s: String): Instruction = {
    val letter = s.charAt(0)
    val amount = s.tail.toInt

    letter match {
      case 'N' => North(amount)
      case 'S' => South(amount)
      case 'E' => East(amount)
      case 'W' => West(amount)
      case 'F' => Forward(amount)
      case 'L' => Left(amount % 360)
      case 'R' => Right(amount % 360)
    }
  }

  sealed trait Instruction

  case class North(distance: Int) extends Instruction

  case class South(distance: Int) extends Instruction

  case class East(distance: Int) extends Instruction

  case class West(distance: Int) extends Instruction

  case class Forward(distance: Int) extends Instruction

  case class Left(angle: Int) extends Instruction

  case class Right(angle: Int) extends Instruction

}
