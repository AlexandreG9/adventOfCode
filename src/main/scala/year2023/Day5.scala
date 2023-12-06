package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readWholeFile

import scala.collection.immutable

object Day5 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day5.txt"

  case class Seed(value: Long)

  case class Range(destinationStart: Long, sourceStart: Long, length: Long) {
    def isSupportedInput(value: Long): Boolean = value >= sourceStart && value <= sourceStart + length

    def translate(value: Long): Long = {
      val delta = value - sourceStart
      destinationStart + delta
    }
  }

  case class AlmanacMap(origin: String, destination: String, ranges: List[Range]) {
    def translate(value: Long): Long = {
      val maybeRange = ranges.find(_.isSupportedInput(value))
      maybeRange match {
        case Some(range) => range.translate(value)
        case None => value
      }
    }
  }

  case class Almanac(maps: List[AlmanacMap]) {
    def forwardAll(origin: String, value: Long): Long = {
      println(s"forwardAll: $origin $value")
      val maybeMap = maps.find(_.origin == origin)
      maybeMap match {
        case Some(map) => forwardAll(map.destination, map.translate(value))
        case None => value
      }
    }
  }

  def readSeeds(file: String): List[Seed] = {
    val seeds = file.split("\n").head.replace("seeds: ", "").split(" ").map(_.toLong).toList
    seeds.map(Seed)
  }

  def readSeedsWithRange(file: String): Set[Seed] = {
    val seedsPair = file.split("\n").head.replace("seeds: ", "")
    val regexPair = """(\d+ \d+)""".r

    regexPair.findAllMatchIn(seedsPair).flatMap(m => {
      val values = m.group(1).split(" ")
      val start = values(0).toLong
      val end = values(1).toLong
      start to end map Seed
    }).toSet
  }

  def readAlmanac(file: String): Almanac = {
    val regexDeLaMort = """([a-z]+)-to-([a-z]+) map:\n((\d+(\s|\n))+)""".r
    val almanacMap = regexDeLaMort.findAllMatchIn(file).map(m => {
      val origin = m.group(1)
      val destination = m.group(2)
      val ranges = m.group(3).split("\n").map(_.trim).filter(!_.isBlank).map(line => {
        val values = line.split(" ")
        val destinationStart = values(0).toLong
        val sourceStart = values(1).toLong
        val length = values(2).toLong

        Range(destinationStart, sourceStart, length)
      }).toList

      AlmanacMap(origin, destination, ranges)
    }).toList


    Almanac(almanacMap)
  }


  override def run: IO[Unit] = {
    for {
      file <- readWholeFile(path)
      almanac = readAlmanac(file)
      seeds = readSeedsWithRange(file)
      _ <- IO {
        println(s"number of seeds ${seeds.size}")
      }
      seedsLocation = seeds.map(seed => almanac.forwardAll("seed", seed.value))
      _ <- IO {
        println(seedsLocation.min)
      }
    } yield ()
  }
}
