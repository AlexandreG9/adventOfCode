package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{readLine, readWholeFile}
import year2023.HandType.HandType

import scala.collection.immutable.{NumericRange, Set}


// Not 251785912
// Not 251785912
object Day7Part2 extends IOApp.Simple {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day7.txt"

  case class Card(name: String, value: Int) extends Comparable[Card] {
    override def compareTo(o: Card): Int = value.compareTo(o.value)
  }

  val cardSet = List(
    Card("J", 1),
    Card("2", 2),
    Card("3", 3),
    Card("4", 4),
    Card("5", 5),
    Card("6", 6),
    Card("7", 7),
    Card("8", 8),
    Card("9", 9),
    Card("T", 10),
    Card("Q", 11),
    Card("K", 12),
    Card("A", 13)
  )

  case class Hand(value: String) extends Comparable[Hand] {
    private val valueArr = value.toCharArray.groupBy(identity).filter(_._1 != 'J').values.map(_.size)
    private val numberOfJoker = value.toCharArray.count(_ == 'J')

    def getHandType(): HandType = {
      println(s"get hand type for: ${value}")

      def evaluateCards(cardsType: List[Int]): HandType = {

        case class HandTypeResolver(maybeFullHouse: Boolean, maybeThreeOfAKind: Boolean, maybeTwoPair: Boolean, valueFound: Option[HandType])

        val list = cardsType.sorted.reverse
        val listWithJoker = list.head + numberOfJoker :: list.tail

        println(s"listWithJoker $listWithJoker")

        val maybeFind = listWithJoker.foldLeft(HandTypeResolver(false, false, false, None)) { case (acc, value) =>
          acc.valueFound match {
            case Some(_) => acc
            case None =>
              if (value == 4) acc.copy(valueFound = Some(HandType.FourOfAKind))
              else if (value == 3) acc.copy(maybeFullHouse = true, maybeThreeOfAKind = true)
              else if (value == 2) {
                if (acc.maybeFullHouse) acc.copy(valueFound = Some(HandType.FullHouse))
                else if (acc.maybeTwoPair) acc.copy(valueFound = Some(HandType.TwoPairs))
                else acc.copy(maybeTwoPair = true)
              } else acc
          }
        }

        maybeFind.valueFound match {
          case Some(value) => value
          case None => if (maybeFind.maybeThreeOfAKind) {
            HandType.ThreeOfAKind
          } else if (maybeFind.maybeTwoPair) {
            HandType.OnePair
          } else ???
        }
      }

      val handType = if (valueArr.size == 5) HandType.HighCard // No joker + 5 different cards
      else if (valueArr.size == 0) HandType.FiveOfKind // 5 joker
      else if (valueArr.size == 1) HandType.FiveOfKind // 4 cards + 1 joker
      else evaluateCards(valueArr.toList.sorted.reverse)

      println(s"founded handType : ${handType}")
      handType
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
      value = orderedBets.zipWithIndex.map { case (bet, index) => bet.bid * (index + 1) }.sum
      _ <- IO(println(value))
    } yield ()
  }
}
