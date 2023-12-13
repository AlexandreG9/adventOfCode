package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

object Day9 extends IOApp.Simple {

  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day9.txt"

  def findNextNumber(numbers: List[Int]): Int = {
    def getIntervals(numbers: List[Int]): List[Int] =
      numbers.sliding(2).map(l => l.head - l.last).toList


    def loop(acc: List[List[Int]]): List[List[Int]] = {
        if (acc.last.forall(v => v == 0)) acc
        else {
          val nextIntervals = getIntervals(acc.last)
          loop(acc :+ nextIntervals)
        }
    }

    ???
  }

  override def run: IO[Unit] = for {
    lines <- readLine(path)
    example = List(10, 13, 16, 21, 30, 45)
    foundValue = findNextNumber(example)
    _ <- IO(println(s"foundValue: $foundValue"))
    _ <- IO(println("expected value is 68"))
  } yield ()

}
