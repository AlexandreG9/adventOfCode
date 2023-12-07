package year2023

import cats.effect.{IO, IOApp}
import cats.implicits._
import year2022.utils.FileUtils.readWholeFile

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.Duration

object Day6 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day6.txt"

  case class RaceTry(race: Race, durationHoldButton: Long, durationReleaseButton: Long) {
    def canBeatRecordDistance(): Boolean = {
      val speed = durationHoldButton
      val traveledDistance = durationReleaseButton * speed

      traveledDistance > race.distance
    }
  }

  case class Race(time: Long, distance: Long) {
    private def getPossibleRaceTry(): List[RaceTry] = {
      val possibleDurationHoldButton = NumericRange.inclusive(0L, time, 1L).toList

      possibleDurationHoldButton.map(duration =>
        RaceTry(this, duration, time - duration)
      )
    }

    def countPossibleWins: Long = {
      val raceTryList = getPossibleRaceTry()
      raceTryList.count(_.canBeatRecordDistance())
    }
  }

  def readRace(file: String): List[Race] = {
    val lines = file.split("\n").toList

    val findNumberRegex = """\W+(\d+)""".r
    val allTimes = findNumberRegex.findAllMatchIn(lines(0))
    val allDistances = findNumberRegex.findAllMatchIn(lines(1))

    allTimes.zip(allDistances).map(elem => {
      Race(elem._1.group(1).toLong, elem._2.group(1).toLong)
    }).toList
  }

  def readBigRace(file: String): Race = {
    val lines = file.split("\n").toList

    val findNumberRegex = """\W+(\d+)""".r
    val time = findNumberRegex.findAllMatchIn(lines.head).map(_.group(1)).mkString("").toLong
    val distance = findNumberRegex.findAllMatchIn(lines(1)).map(_.group(1)).mkString("").toLong

    Race(time, distance)
  }

  override def run: IO[Unit] = {
    for {
      file <- readWholeFile(path)
      races = readRace(file)
      bigRace = readBigRace(file)
      possibleWins = races.map(_.countPossibleWins).product
      _ <- IO {
        println(races)
        println(s"possible wins: $possibleWins")
        println(s"big race possible wins: ${bigRace.countPossibleWins}")
      }
    } yield ()
  }
}
