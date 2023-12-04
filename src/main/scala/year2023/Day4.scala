package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day4 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day4.txt"

  case class Card(id: Int, winningNumbers: List[Int], numbers: List[Int])

  def readCard(line: String): Card = {
    val cardId = """Card +(\d+)""".r.findFirstMatchIn(line).map(_.group(1).toInt)

    val extractNumbersRegex = """Card +\d+:""".r
    val cardsNumbers = line.replaceAll(extractNumbersRegex.toString(), "").split("\\|").map(_.trim)

    val winningNumbers = cardsNumbers(0).split(" ").map(_.trim).filter(!_.isBlank).map(_.toInt).toList
    val numbers = cardsNumbers(1).split(" ").map(_.trim).filter(!_.isBlank).map(_.toInt).toList

    Card(cardId.getOrElse(0), winningNumbers, numbers)
  }

  def getNextIndexes(actualIndex: Int, matchingNumber: Int): List[Int] = {
    def loop(acc: List[Int], remainingNumber: Int): List[Int] = {
      if (remainingNumber == 0) acc
      else loop(acc.head + 1 :: acc, remainingNumber - 1)
    }

    loop(List(actualIndex), matchingNumber)
  }

  def calculatePoint(card: Card, allCards: List[Card], recursively: Boolean = false): Int = {
    println(s"card : ${card.id}")
    val matchingNumbers = card.numbers.count(card.winningNumbers.contains(_))
    println(s"matchingNumbers : ${matchingNumbers}")
    val actualPoint = if (matchingNumbers > 1) {
      Math.pow(2, matchingNumbers - 1).toInt
    } else {
      matchingNumbers
    }

    if (!recursively) actualPoint
    else {
      val nextIds = getNextIndexes(card.id, matchingNumbers)
      val otherCards = allCards.filter(c => nextIds.contains(c.id))
      val otherPoints = otherCards.map(calculatePoint(_, allCards)).sum

      otherPoints
    }

  }


  override def run: IO[Unit] =
    for {
      lines <- readLine(path)
      cards = lines.map(readCard).toList
      points = cards.map(calculatePoint(_, cards, recursively = true)).sum
      _ <- IO(println(cards))
      _ <- IO(println(points))
    } yield ()
}
