package year2023

import cats.effect.{IO, IOApp}
import cats.implicits._
import year2022.utils.FileUtils.readWholeFile

import scala.concurrent.duration.Duration

object Day5_BruteForce extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day5.txt"

  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  case class Seed(value: Long) {
    def transformToSeedRange(): SeedRange = {
      SeedRange(value, 1)
    }
  }

  case class SeedWithResult(value: Long, result: Long)

  case class SeedRange(start: Long, end: Long) {
    def contains(value: Long): Boolean = value >= start && value <= end

    def intersectRange(other: SeedRange): Option[SeedRange] = {
      if (other.start > end || other.end < start) {
        None
      } else {
        val newStart = Math.max(start, other.start)
        val newEnd = Math.min(end, other.end)
        Some(SeedRange(newStart, newEnd))
      }
    }

    override def toString: String = s"($start -> $end)"
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
      // println(s"forwardAll: $origin $value")
      val maybeMap = maps.find(_.origin == origin)
      maybeMap match {
        case Some(map) => forwardAll(map.translate(value), map.destination)
        case None => value
      }
    }

    def hardComputeMinLocationFromSeedRange(seedRange: SeedRange): IO[SeedWithResult] = IO {
      def loop(currentMinSeed: SeedWithResult, currentSeed: Long): SeedWithResult = {
        if (currentSeed > seedRange.end) {
          currentMinSeed
        } else {
          val currentSeedWithResult = SeedWithResult(currentSeed, forwardAll(currentSeed))

          val minSeed = if (currentMinSeed.result < currentSeedWithResult.result) {
            currentMinSeed
          } else {
            currentSeedWithResult
          }

          loop(minSeed, currentSeed + 1)
        }
      }

      loop(SeedWithResult(seedRange.start, forwardAll(seedRange.start)), seedRange.start)
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

    def findFitRangeForSeedRange(seedRange: SeedRange): Unit = {
      def getNonMatchingIntersectionRange(seedRange: SeedRange, fitRange: List[SeedRange]): List[SeedRange] = {
        // Need to rework, Output must be (45, 50)
        val minLimit = seedRange.start
        val maxLimit = seedRange.end

        val sortedFitRange = fitRange.sortBy(_.start)
        sortedFitRange.foldLeft((List.empty[SeedRange], None: Option[SeedRange]))((acc, range) => {
          val (accRanges, maybePreviousRange) = acc

          maybePreviousRange match {
            case Some(lastRange) => {
              if (lastRange.end < range.start) {
                val newRange = SeedRange(lastRange.end, Math.min(range.start, maxLimit))
                ((accRanges :+ newRange), Some(range))
              } else {
                (accRanges, Some(range))
              }
            }
            case None => {
              if (minLimit < range.start) {
                val newRange = SeedRange(seedRange.start, Math.min(seedRange.end, maxLimit))
                ((accRanges :+ newRange), Some(range))
              } else {
                (accRanges, Some(range))
              }
            }
          }

        })._1
      }


      val seedMapRange = maps.find(_.origin == "seed").get.ranges.map(range => {
        val start = range.sourceStart
        val end = range.sourceStart + range.length
        SeedRange(start, end)
      })

      // split range if it is not in one range
      val intersectionRangeSeedMatch = seedMapRange.flatMap(range => {
        seedRange.intersectRange(range)
      })

      val intersectionWhitoutRangeMatch = getNonMatchingIntersectionRange(seedRange, intersectionRangeSeedMatch)
      println(s"intersectionWhitoutRangeMatch: $intersectionWhitoutRangeMatch")
      println(s"seedMapRange: $intersectionRangeSeedMatch")

    }

    /**
     * def findIntervalForMinimumLocation(): InputRange = {
     * val locationMap = maps.find(_.destination == "location").get
     * val minDestinationRange = locationMap.ranges.sortBy(_.destinationStart).flatMap(possibleMinRange => {
     * val foundedSeeds = (possibleMinRange.sourceStart to possibleMinRange.sourceStart + possibleMinRange.length).map(upwardAll(_)).toList
     * foundedSeeds
     * }).toSet
     *
     * println(s"foundedSeeds: ${minDestinationRange.size}")
     * println(s"foundedSeeds: ${minDestinationRange}")
     * ???
     * }
     */
  }

  def readSeeds(file: String): List[Seed] = {
    val seeds = file.split("\n").head.replace("seeds: ", "").split(" ").map(_.toLong).toList
    seeds.map(Seed)
  }


  def readSeedsWithRange(file: String): List[SeedRange] = {
    val seedsPair = file.split("\n").head.replace("seeds: ", "")
    val regexPair = """(\d+ \d+)""".r

    regexPair.findAllMatchIn(seedsPair).map(m => {
      val values = m.group(1).split("\\s")
      val start = values(0).toLong
      val end = start + (values(1).toLong - 1)
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
      // seedRange = readSeeds(file).map(_.transformToSeedRange())
      _ <- IO(println("--- Start ---"))
      _ <- IO(println(s"seedRange: ${seedRange.mkString(" ")})}"))
      currentMillis = System.currentTimeMillis()
      listMinSeed <- seedRange.map(almanac.hardComputeMinLocationFromSeedRange).parSequence
      _ <- IO(println(s"listMinSeed: ${listMinSeed}"))
      minSeed = listMinSeed.minBy(_.result)
      _ <- IO(println("--- Finished ---"))
      _ <- IO(println(s"Time taken: ${System.currentTimeMillis() - currentMillis} ms"))
      _ <- IO(println(s"minLocation: ${minSeed}"))
    } yield ()
  }
}
