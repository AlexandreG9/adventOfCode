package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day5 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day5.txt"

  case class Range(destinationStart: Int, sourceStart: Int, length: Int)

  case class AlmanacMap(origin: String, destination: String, ranges: List[Range])

  case class Almanac(seed: List[Int], maps: List[AlmanacMap])

  def readAlmanac(file: String): Almanac = {
    val regexDeLaMort = """([a-z]+)-to-([a-z]+) map:\n((\d+(\s|\n))+)""".r

    val seeds = file.split("\n").head.replace("seeds: ", "").split(" ").map(_.toInt).toList

    val almanacMap = regexDeLaMort.findAllMatchIn(file).map(m => {
      val origin = m.group(1)
      val destination = m.group(2)
      val ranges = m.group(3).split("\n").map(_.trim).filter(!_.isBlank).map(line => {
        val values = line.split(" ")

        val destinationStart = values(0).toInt
        val sourceStart = values(1).toInt
        val length = values(2).toInt

        Range(destinationStart, sourceStart, length)
      }).toList

      AlmanacMap(origin, destination, ranges)
    }).toList


    Almanac(seeds, almanacMap)
  }


  override def run: IO[Unit] = {

    for {
      file <- readWholeFile(path)
      almanac = readAlmanac(file)
      _ <- IO {
        println(almanac)
      }
    } yield ()
  }
}
