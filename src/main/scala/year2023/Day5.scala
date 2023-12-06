package year2023

import year2022.utils.FileUtils.readWholeFile

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex
import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.Duration

object Day5 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day5.txt"

  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  case class Seed(value: Long)

  case class SeedRange(start: Long, end: Long) {
    def contains(value: Long): Boolean = value >= start && value <= end
  }

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
    def forwardAll(value: Long, origin: String = "seed"): Long = {
      println(s"forwardAll: $origin $value")
      val maybeMap = maps.find(_.origin == origin)
      maybeMap match {
        case Some(map) => forwardAll(map.translate(value), map.destination)
        case None => value
      }
    }

    // not used
    def upwardAll(value: Long, destination: String = "location"): Long = {
      // println(s"upwardAll: $destination $value")
      val maybeMap = maps.find(_.destination == destination)
      maybeMap match {
        case Some(map) => upwardAll(map.translate(value), map.origin)
        case None => value
      }
    }
 /**
    def findIntervalForMinimumLocation(): InputRange = {
      val locationMap = maps.find(_.destination == "location").get
      val minDestinationRange = locationMap.ranges.sortBy(_.destinationStart).flatMap(possibleMinRange => {
        val foundedSeeds = (possibleMinRange.sourceStart to possibleMinRange.sourceStart + possibleMinRange.length).map(upwardAll(_)).toList
        foundedSeeds
      }).toSet

      println(s"foundedSeeds: ${minDestinationRange.size}")
      println(s"foundedSeeds: ${minDestinationRange}")
      ???
    }
*/
  }

  def readSeeds(file: String): List[Seed] = {
    val seeds = file.split("\n").head.replace("seeds: ", "").split(" ").map(_.toLong).toList
    seeds.map(Seed)
  }


  def readSeedsWithRange(file: String): List[SeedRange] = {
    val seedsPair = file.split("\n").head.replace("seeds: ", "")
    println(s"seedsPair: $seedsPair")
    val regexPair = """(\d+ \d+)""".r

    regexPair.findAllMatchIn(seedsPair).map(m => {
      val values = m.group(1).split("\\s")
      val start = values(0).toLong
      val end = start + values(1).toLong
      SeedRange(start, end)
    }).toList
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

  def countPossibleSeedsFromInput(inputRange: SeedRange): Long = {
    inputRange.end - inputRange.start + 1
  }


  override def run: IO[Unit] = {
    for {
      file <- readWholeFile(path)
      almanac = readAlmanac(file)
      seedRange = readSeedsWithRange(file)
      possibleSeeds = seedRange.map(countPossibleSeedsFromInput).sum
      _ = println(s"possibleSeeds: $possibleSeeds")

    } yield ()
  }
}
