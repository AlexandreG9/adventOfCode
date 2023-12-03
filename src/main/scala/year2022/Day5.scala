package year2022

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

import scala.util.matching.Regex

val stack1 = List("H", "L", "R", "F", "B", "C", "J", "M")
val stack2 = List("D", "C", "Z")
val stack3 = List("W", "G", "N", "C", "F", "J", "H")
val stack4 = List("B", "S", "T", "M", "D", "J", "P")
val stack5 = List("J", "R", "D", "C", "N")
val stack6 = List("Z", "G", "J", "P", "Q", "D", "L", "W")
val stack7 = List("H", "R", "F", "T", "Z", "P")
val stack8 = List("G", "M", "V", "L")
val stack9 = List("J", "R", "Q", "F", "P", "G", "B", "C")

object Day5 extends IOApp.Simple {

  val shipCrates = List(stack1, stack2, stack3, stack4, stack5, stack6, stack7, stack8, stack9)


  val instructionRegex: Regex= "\\d+".r
  def computeInstruction(instruction: String, shipCratesState: List[List[String]]): List[List[String]] = {
    instructionRegex.findAllMatchIn(instruction)

    ???
  }


  val run = for {
    lines <- readLine("/Users/alexandre/IdeaProjects/adventOfCode/src/resources/2022/input-day5.txt")
  } yield ()
}
