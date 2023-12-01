package year2022

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine


case class Rucksack(comp1: String, comp2: String) {
  def getCommonItem: Option[Char] =
    comp1.toCharArray.find(char => comp2.contains(char))
}

object Rucksack {
  def readFromString(str: String): Rucksack = {
    val (comp1, comp2) = str.splitAt(str.length / 2)
    Rucksack(comp1, comp2)
  }
}


object Day3 extends IOApp.Simple {
  private val LOWER_INDEX = 97 - 1
  private val UPPER_INDEX = (65 - 1) - 26

  def getItemScore(item: Char): Int =
    if (item.isUpper) item.toInt - UPPER_INDEX else item.toInt - LOWER_INDEX

  def splitIntoGroup(rucksack: List[Rucksack]): List[List[Rucksack]] = rucksack.grouped(3).toList

  def findGroupBadge(group: List[Rucksack]): Option[Char] =
    if (group.size != 3) throw new Exception("Can only be 3 in group")
    else {
      val groupStr = group.map(r => r.comp1 + r.comp2)
      val firstElem = groupStr.head
      val otherElem = groupStr.tail

      firstElem.toCharArray.find(c => otherElem.count(s => s.contains(c)) == 2)
    }

  val run = for {
    lines <- readLine("/Users/alexandre/IdeaProjects/adventOfCode/src/resources/2022/input-day3.txt")
    rucksacks = lines.map(Rucksack.readFromString)
    commonItem = rucksacks.map(_.getCommonItem).map(_.map(getItemScore))
    sum = commonItem.flatten.sum
    groupedRucksacks = splitIntoGroup(rucksacks.toList)
    sumBadge = groupedRucksacks.flatMap(findGroupBadge)
      .map(getItemScore)
      .sum
    _ <- IO.println(s"sum: $sum")
    _ <- IO.println(s"sum group badge: $sumBadge")
  } yield ()
}
