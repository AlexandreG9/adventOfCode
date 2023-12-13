package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

object Day10 extends IOApp.Simple {

  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day10.txt"



  override def run: IO[Unit] = for {
    lines <- readLine(path)
  } yield ()

}
