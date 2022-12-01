package year2022

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.{inputStream, readLine}


case class ElvesFood(calories: Int)
object ElvesFood {
  def fromListFood(foodList: Seq[String]): ElvesFood = {
    val calories = foodList.map(_.toInt).sum
    ElvesFood(calories)
  }
}

object Day1 extends IOApp.Simple {
  val run =
    for {
      lines <- readLine("/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/src/resources/2022/input-day1.txt")
      elves = readElvesFood(lines)
      maxCalories = elves.map(_.calories).max
      top3Calories = elves.map(_.calories).sorted.reverse.take(3).sum
      _ <- IO.println(s"maxCalories: $maxCalories")
      _ <- IO.println(s"top3Calories: $top3Calories")
    } yield ()

  def readElvesFood(inputLine: Seq[String]): Seq[ElvesFood] = {
    inputLine.foldLeft((Nil: Seq[ElvesFood], Nil: Seq[String]))((acc, line) => {
      line match {
        case "" => ((acc._1 ++ Seq(ElvesFood.fromListFood(acc._2))), Seq.empty)
        case value => (acc._1, acc._2  :+ value)
      }
    })._1
  }
}
