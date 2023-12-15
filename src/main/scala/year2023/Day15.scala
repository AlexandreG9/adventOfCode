package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

object Day15 extends IOApp.Simple {

  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day15.txt"

  def hashAlgorithm(text: String): Int =
    text.toCharArray.toList.foldLeft(0)((acc, char) => ((acc + char.toInt) * 17) % 256)

  override def run: IO[Unit] = for {
    line <- readLine(path)
    lines = line.head.split(",")
    resultPart1 = lines.map(hashAlgorithm).sum
    _ <- IO(println(s"part1: ${resultPart1}"))
    _ <- IO(println("part2: "))
  } yield ()
}
