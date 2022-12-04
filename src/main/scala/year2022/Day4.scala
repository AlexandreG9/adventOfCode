package year2022

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

case class Sector(start: Int, end: Int)

object Sector {
  def fromString(str: String): Sector = {
    val Array(start, end) = str.split('-')
    Sector(start.toInt, end.toInt)
  }

  def fullyContains(s1: Sector, s2: Sector): Boolean =
    s1.start <= s2.start && s1.end >= s2.end

  def overlap(s1: Sector, s2: Sector): Boolean =
    s1.start <= s2.start && s1.end >= s2.start
}

case class Pair(sector1: Sector, sector2: Sector) {
  def sectorFullyContainsOther: Boolean =
    Sector.fullyContains(sector1, sector2) || Sector.fullyContains(sector2, sector1)

  def sectorOverlapsOther: Boolean =
    Sector.overlap(sector1, sector2) || Sector.overlap(sector2, sector1)

}

object Pair {
  def getPairFromString(str: String): Pair = {
    val Array(first, second) = str.split(',')

    Pair(Sector.fromString(first), Sector.fromString(second))
  }

}

object Day4 extends IOApp.Simple {

  val run = for {
    lines <- readLine("/Users/alexandre/IdeaProjects/adventOfCode/src/resources/2022/input-day4.txt")
    pair = lines.map(Pair.getPairFromString)
    countFullyContains = pair.count(_.sectorFullyContainsOther)
    countOverlap = pair.count(_.sectorOverlapsOther)
    _ <- IO.println(s"total full contains: $countFullyContains")
    _ <- IO.println(s"total overlap: $countOverlap")
  } yield ()
}
