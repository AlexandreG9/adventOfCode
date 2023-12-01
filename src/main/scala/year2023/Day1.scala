package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

object Day1 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day1.txt"

  private val digitsTab: Seq[(String, Int)] =
    Seq(
      "one|1" -> 1,
      "two|2" -> 2,
      "three|3" -> 3,
      "four|4" -> 4,
      "five|5" -> 5,
      "six|6" -> 6,
      "seven|7" -> 7,
      "eight|8" -> 8,
      "nine|9" -> 9,
    )

  case class SingleCalibration(pos: Int, value: Int)

  /**
   * Replace word by digit.
   * Example : fourTtt13 as input will return 413
   * @param line input
   * @return line with digit
   */
  def interprateDigit(line: String): String = {
    digitsTab.map(tuple => {
        tuple._1.r.findAllMatchIn(line) -> tuple._2
      }).flatMap(m =>
        m._1.map(group => SingleCalibration(group.start, m._2)))
      .sortBy(_.pos)
      .map(_.value)
      .mkString("")
  }

  private def extractCalibrationValue(line: String): Int = {
    val pattern = """\d""".r
    val allDigits = pattern.findAllMatchIn(line).toList
    val firstDigit = allDigits.head
    val lastDigit = allDigits.last

    s"$firstDigit$lastDigit".toInt
  }


  override def run: IO[Unit] =
    for {
      lines <- readLine(path)
      _ <- IO.println(s"lines: ${lines.size}")
      basicCalibrationValues = lines.map(extractCalibrationValue)
      complexCalibrationValues = lines.map(interprateDigit).map(extractCalibrationValue).sum
      _ <- IO.println(s"basic calibration values: ${basicCalibrationValues.sum}")
      _ <- IO.println(s"computeValidLines: ${complexCalibrationValues}")
      _ <- IO.println(s"sum: ${basicCalibrationValues.sum + complexCalibrationValues}")
    } yield ()
}
