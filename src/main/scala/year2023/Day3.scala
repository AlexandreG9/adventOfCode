package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day3 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day3.txt"

  case class EngineNumber(start: Int, end: Int, line: Int, value: Int)

  case class Position(x: Int, y: Int)

  case class Gear(value: Int, position: Position)

  def findNumber(file: String): List[EngineNumber] = {
    val regex = """(\d+)""".r

    val lines = file.split("\n")


    lines.zipWithIndex.flatMap { case (line, index) =>
      regex.findAllMatchIn(line).map(m => {
        EngineNumber(m.start, m.end, index, m.group(0).toInt)
      }).toList
    }.toList
  }

  /**
   * Check if the number is rounded by symbol in the file
   */
  def checkValidNumber(engineNumber: EngineNumber, file: String): Boolean = {
    val maxChar = file.split("\n")(engineNumber.line).length

    // println(s"engineNumber: ${engineNumber.line} ${engineNumber.start} ${engineNumber.end}")

    val lineAbove = if (engineNumber.line > 0) getCharAtPosition(Math.max(engineNumber.start - 1, 0), Math.min(engineNumber.end + 1, maxChar), engineNumber.line - 1, file) else ""
    val lineBelow = if (engineNumber.line < file.split("\n").length - 1) getCharAtPosition(Math.max(engineNumber.start - 1, 0), Math.min(engineNumber.end + 1, maxChar), engineNumber.line + 1, file) else ""
    val charBeforOnSameLine = if (engineNumber.start > 0) getCharAtPosition(engineNumber.start - 1, engineNumber.start, engineNumber.line, file) else ""
    val charAfterOnSameLine = if (engineNumber.end < file.split("\n")(engineNumber.line).length - 1) getCharAtPosition(engineNumber.end, engineNumber.end + 1, engineNumber.line, file) else ""

    val allChars = lineAbove + lineBelow + charBeforOnSameLine + charAfterOnSameLine

    val regex = """[^\d.\w]""".r
    regex.findAllMatchIn(allChars).nonEmpty
  }

  def getCharAtPosition(start: Int, end: Int, line: Int, file: String): String = {
    val lines = file.split("\n")
    val lineToCheck = lines(line)
    lineToCheck.substring(start, end)
  }

  def findGearPosition(file: String): List[Position] = {
    val regex = """(\*)""".r
    val lines = file.split("\n")

    lines.zipWithIndex.flatMap { case (line, index) =>
      regex.findAllMatchIn(line).map(m => {
        Position(m.start, index)
      }).toList
    }.toList
  }

  def findGearValue(gearPosition: Position, engineNumber: List[EngineNumber], file: String): Option[Gear] = {
    println(s"searching for gear: ${gearPosition}")

    val maxChar = file.split("\n")(gearPosition.y).length
    val maybeUpperEngineNumbers = if (gearPosition.y > 0) {
      findEngineNumberWithPosition(Math.max(gearPosition.x - 1, 0), Math.min(gearPosition.x + 1, maxChar), gearPosition.y - 1, engineNumber)
    } else Nil
    val maybeDownEngineNumbers = if (gearPosition.y < maxChar) {
      findEngineNumberWithPosition(Math.max(gearPosition.x - 1, 0), Math.min(gearPosition.x + 1, maxChar), gearPosition.y + 1, engineNumber)
    } else Nil
    val maybeLeft = if (gearPosition.x > 0) {
      findEngineNumberWithPosition(gearPosition.x - 1, gearPosition.x - 1, gearPosition.y, engineNumber)
    } else Nil
    val maybeRight = if (gearPosition.x < maxChar) {
      findEngineNumberWithPosition(gearPosition.x + 1, gearPosition.x + 1, gearPosition.y, engineNumber)
    } else Nil


    val maybeNumber = maybeUpperEngineNumbers ++ maybeDownEngineNumbers ++ maybeLeft ++ maybeRight

    println("founded numbers: " + maybeNumber)

    maybeNumber match {
      case numbersList if numbersList.size == 2 => {
        val value = numbersList.map(_.value).product
        println(s"Numbers: ${numbersList}")
        Some(Gear(value, gearPosition))
      }
      case _ => None
    }
  }

  def findEngineNumberWithPosition(start: Int, end: Int, line: Int, engineNumber: List[EngineNumber]): List[EngineNumber] =
    engineNumber.filter(engineNumber => engineNumber.line == line &&
      ((engineNumber.start >= start && engineNumber.start <= end) ||
        ((engineNumber.end - 1) >= start && (engineNumber.end - 1)  <= end)))

  override def run: IO[Unit] =
    for {
      file <- readWholeFile(path)
      // _ <- IO.println(s"${file}")
      engineNumberList = findNumber(file)
      validEngineList = engineNumberList.filter(engineNumber => checkValidNumber(engineNumber, file))
      sumValue = validEngineList.map(_.value).sum
      gears = findGearPosition(file)
      validGears = gears.flatMap(gearPosition => findGearValue(gearPosition, engineNumberList, file))
      _ <- IO.println(s"Engine number list: ${engineNumberList.map(_.value)}")
      // _ <- IO.println(s"${validEngineList.map(_.value)}")
      // _ <- IO.println(s"${sumValue}")
      // _ <- IO.println(s"${gears}")
      _ <- IO.println(s"Valid gears ${validGears}")
      _ <- IO.println(s"Sum of valid gears ${validGears.map(_.value).sum}")
    } yield ()
}
