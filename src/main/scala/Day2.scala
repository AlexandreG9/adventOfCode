import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int, aim: Int) {
  def getValue(): Int = x * y
}


object Main extends App {
  val filename = "input-day2.txt"
  val input = Source.fromFile(filename).getLines.toList

  val finalPosition = loop(Position(0, 0, 0), input)

  println(finalPosition)
  println("Value=" + finalPosition.getValue())


  @tailrec
  def loop(position: Position, listInput: List[String]): Position = {
    if (listInput.isEmpty) position
    else loop(readInput(position, listInput.head), listInput.tail)
  }

  def readInput(actualPosition: Position, input: String): Position = {
    val value = input.split(" ")(1).toInt
    val operation = input.split(" ")(0)
    operation match {
      case "forward" => Position(actualPosition.x + value, actualPosition.y + calculateDepth(actualPosition.aim, value), actualPosition.aim)
      case "up" => Position(actualPosition.x, actualPosition.y, actualPosition.aim - value)
      case "down" => Position(actualPosition.x, actualPosition.y, actualPosition.aim + value)
    }
  }

  def calculateDepth(aim: Int, value: Int): Int = aim * value
}