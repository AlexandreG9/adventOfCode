package year2023

import year2022.utils.FileUtils.readWholeFile

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex
import cats.effect.{ IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.Duration
object Day5 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day5.txt"

  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

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
    def forwardAll(value: Long, origin: String = "seed"): Long = {
      println(s"forwardAll: $origin $value")
      val maybeMap = maps.find(_.origin == origin)
      maybeMap match {
        case Some(map) => forwardAll(map.translate(value), map.destination)
        case None => value
      }
    }
  }

  def readSeeds(file: String): List[Seed] = {
    val seeds = file.split("\n").head.replace("seeds: ", "").split(" ").map(_.toLong).toList
    seeds.map(Seed)
  }

  def readSeedsWithRange(file: String): IO[List[Seed]] = {
    val seedsPair = file.split("\n").head.replace("seeds: ", "")
    println(s"seedsPair: $seedsPair")
    val regexPair = """(\d+ \d+)""".r

    def computeFromRegexMatches(m: Regex.Match): IO[List[Seed]] = IO {
      val values = m.group(1).split("\\s")
      val start = values(0).toLong
      val end = start + values(1).toLong

      val estimateSize = (end - start) * 8 / 1024 / 1024
      println(s"estimateSize: $estimateSize MB")
      val seeds = NumericRange.inclusive(start, end, 1).toList.map(Seed)
      println(s"seeds created for range: $start")
      seeds
    }

    regexPair.findAllMatchIn(seedsPair).map(m => {
      computeFromRegexMatches(m)
    }).toList.sequence.map(_.flatten)
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
      seeds <- readSeedsWithRange(file)
      _ <- IO {
        println(s"Seeds created, number of seeds ${seeds.size}")
      }
      seedsLocation = seeds.map(seed => almanac.forwardAll(seed.value))
      _ <- IO {
        println(seedsLocation.min)
      }
    } yield ()
  }
}
