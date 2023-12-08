package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

import scala.collection.immutable.NumericRange

object HandType extends Enumeration {
  type HandType = Value
  val HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfKind = Value
}

object Day7Part1 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day7.txt"

  case class Card(name: String, value: Int) extends Comparable[Card] {
    override def compareTo(o: Card): Int = value.compareTo(o.value)
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

  case class Hand(value: String) extends Comparable[Hand] {
    def getHandType() = {
      if (isFiveOfAKind()) HandType.FiveOfKind
      else if (isFourOfAKind()) HandType.FourOfAKind
      else if (isFullHouse()) HandType.FullHouse
      else if (isThreeOfAKind()) HandType.ThreeOfAKind
      else if (isTwoPairs()) HandType.TwoPairs
      else if (isOnePair()) HandType.OnePair
      else HandType.HighCard
    }

    private def isFiveOfAKind() = {
      val card = value.toCharArray
      card.forall(c => c == card(0))
    }

    private def isFourOfAKind() = {
      val card = value.toCharArray
      card.groupBy(identity).view.mapValues(_.size).values.exists(_ == 4)
    }

    private def isFullHouse() = {
      val card = value.toCharArray
      card.groupBy(identity).view.mapValues(_.size).values.exists(_ == 3) && card.groupBy(identity).view.mapValues(_.size).values.exists(_ == 2)
    }

    private def isThreeOfAKind() = {
      val card = value.toCharArray
      card.groupBy(identity).view.mapValues(_.size).values.exists(_ == 3)
    }

    private def isTwoPairs() = {
      val card = value.toCharArray
      card.groupBy(identity).view.mapValues(_.size).values.count(_ == 2) == 2
    }

    private def isOnePair() = {
      val card = value.toCharArray
      card.groupBy(identity).view.mapValues(_.size).values.exists(_ == 2)
    }

    override def compareTo(o: Hand): Int = {
      val handType = getHandType()
      val otherHandType = o.getHandType()

      if (handType == otherHandType) {
        val card = value.toCharArray.map(c => cardSet.find(_.name == c.toString).get)
        val otherCard = o.value.toCharArray.map(c => cardSet.find(_.name == c.toString).get)
        card.zip(otherCard).map { case (c, o) => c.compareTo(o) }.find(_ != 0).getOrElse(0)
      } else {
        handType.compareTo(otherHandType)
      }
    }
  }

  case class Bet(hand: Hand, bid: Int)

  def parseLine(line: String): Bet = {
    val split = line.split(" ")
    Bet(Hand(split.head), split.last.toInt)
  }

  override def run: IO[Unit] = {
    for {
      file <- readLine(path)
      bets = file.map(parseLine)
      orderedBets = bets.sortBy(_.hand)
      value = orderedBets.zipWithIndex.map {case (bet, index) => bet.bid * (index + 1) }.sum
      _ <- IO(println(value))
    } yield ()
  }
}
