package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}

object Day4 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day4.txt"

  case class Card(id: Int, winningNumbers: List[Int], numbers: List[Int]) {
    def getMatchingNumbers(): Int = numbers.count(winningNumbers.contains(_))

    def getPoints(): Int = getMatchingNumbers() match {
      case value if value <= 1 => value
      case value => Math.pow(2, value - 1).toInt
    }
  }

  case class CardSimulation(computedCards: List[Card], newCards: List[Card])

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
      if (remainingNumber == 0) acc.reverse
      else loop(acc.head + 1 :: acc, remainingNumber - 1)
    }

    loop(List(actualIndex), matchingNumber)
  }

  def countCard(currentCard: Card, allCardsInGame: List[Card]): Int = {
    val matchingNumbers = currentCard.getMatchingNumbers()

    def countNextCards = {
      if (matchingNumbers == 0) 0
      else {
        val otherIds = getNextIndexes(currentCard.id, matchingNumbers).tail
        val allCards = allCardsInGame.filter(c => otherIds.contains(c.id))
        val otherPoints = allCards.map(countCard(_, allCardsInGame)).sum
        otherPoints
      }
    }

    1 + countNextCards
  }

  def computeCard(currentCard: Card, allCardsInGame: List[Card], recursively: Boolean = false): Int = {
    val matchingNumbers = currentCard.getMatchingNumbers()

    if (!recursively) currentCard.getPoints()
    else {
      if (matchingNumbers > 1) {
        val allIds = getNextIndexes(currentCard.id, matchingNumbers)
        val allCards = allCardsInGame.filter(c => allIds.contains(c.id))
        println(s"card : ${currentCard.id} cards : ${allCards.map(c => s"id: ${c.id} -> ${c.getPoints()}")}")
        val allPoints = allCards.map(computeCard(_, allCardsInGame, true)).sum

        println(s"card : ${currentCard.id} allPoints : ${allPoints}")

        allPoints
      } else {
        currentCard.getPoints()
      }
    }
  }


  override def run: IO[Unit] = {
    val right_answer = 5132675

    for {
      lines <- readLine(path)
      timestamp = System.currentTimeMillis()
      cards = lines.map(readCard).toList
      numberOfCards = cards.map(countCard(_, cards)).sum
      currentTime <- IO(System.currentTimeMillis() - timestamp)
      _ <- IO(println(s"execution time: ${currentTime}ms"))
      _ <- IO(println(numberOfCards))
      _ <- IO(println(s"correct answer: ${numberOfCards == right_answer}"))
    } yield ()
  }
}
