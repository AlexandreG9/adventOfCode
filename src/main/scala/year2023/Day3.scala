package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day3 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day3.txt"

  case class EngineNumber(start: Int, end: Int, line: Int, value: Int)

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

    println(s"engineNumber: ${engineNumber.line} ${engineNumber.start} ${engineNumber.end}")

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

  override def run: IO[Unit] =
    for {
      file <- readWholeFile(path)
      _ <- IO.println(s"${file}")
      engineList = findNumber(file)
      validEngineList = engineList.filter(engineNumber => checkValidNumber(engineNumber, file))
      sumValue = validEngineList.map(_.value).sum
      _ <- IO.println(s"${validEngineList.map(_.value)}")
      _ <- IO.println(s"${sumValue}")
    } yield ()
}
