package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day4 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day4.txt"



  override def run: IO[Unit] =
    for {
      lines <- readLine(path)
    } yield ()
}
