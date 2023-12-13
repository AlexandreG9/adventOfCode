package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

object Day9 extends IOApp.Simple {

  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day9.txt"

  def parseLine(line: String): List[Long] =
    """-?\d+""".r.findAllMatchIn(line).map(m => m.group(0).toLong).toList

  def findNextNumber(numbers: List[Long]): Long = {
    if (numbers.forall(_ == 0L)) 0L
    else numbers.last + findNextNumber(numbers.sliding(2).map(l => l.last - l.head).toList)
  }

  override def run: IO[Unit] = for {
    lines <- readLine(path)
    numbers = lines.map(parseLine)
    valuePart1 = numbers.map(findNextNumber).sum
    valuePart2 = numbers.map(_.reverse).map(findNextNumber).sum
    _ <- IO(println(s"part1: $valuePart1"))
    _ <- IO(println(s"part2: $valuePart2"))
  } yield ()

}
