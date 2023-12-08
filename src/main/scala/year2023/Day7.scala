package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

import scala.collection.immutable.NumericRange

object Day7 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day7.txt"

  case class Card(name: String, value: Int) extends Comparable[Card] {
    override def compareTo(o: Card): Int = value.compareTo(o.value)
  }

  case class Hand(value: String) extends Comparable[Hand] {
    def getHandType() = ???

    override def compareTo(o: Hand): Int = ???
  }

  val cardSet = List(
    Card("2", 2),
    Card("3", 3),
    Card("4", 4),
    Card("5", 5),
    Card("6", 6),
    Card("7", 7),
    Card("8", 8),
    Card("9", 9),
    Card("T", 10),
    Card("J", 11),
    Card("Q", 12),
    Card("K", 13),
    Card("A", 14)
  )

  override def run: IO[Unit] = {
    for {
      file <- readLine(path)
    } yield ()
  }
}
