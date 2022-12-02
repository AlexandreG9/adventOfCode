package year2022

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

trait Result {
  def getScore: Int
}
case object Win extends Result {
  override def getScore: Int = 6
}

case object Draw extends Result {
  override def getScore: Int = 3
}

case object Lose extends Result {
  override def getScore: Int = 0
}

trait GameAction {

  def getActionPoint: Int
  def against(opponentAction: GameAction): Result
}

object GameAction {
  def fromChar(value: Char): GameAction = {
    if (value == 'A' || value == 'X') Rock
    else if (value == 'B' || value == 'Y') Paper
    else Scissors
  }
}

case object Scissors extends GameAction {
  override def against(opponentAction: GameAction): Result = opponentAction match {
    case Paper => Win
    case Rock => Lose
    case Scissors => Draw
    case _ => throw new IllegalArgumentException("unknown value")
  }

  override def getActionPoint: Int = 3
}
case object Rock extends GameAction {
  override def against(opponentAction: GameAction): Result = opponentAction match {
    case Paper => Lose
    case Rock => Draw
    case Scissors => Win
    case _ => throw new IllegalArgumentException("unknown value")
  }

  override def getActionPoint: Int = 1
}
case object Paper extends GameAction {
  override def against(opponentAction: GameAction): Result = opponentAction match {
    case Paper => Draw
    case Rock => Win
    case Scissors => Lose
    case _ => throw new IllegalArgumentException("unknown value")
  }

  override def getActionPoint: Int = 2
}


case class Round(opponentAction: GameAction, myAction: GameAction) {
  def getScore: Int = myAction.against(opponentAction).getScore + myAction.getActionPoint
}

object Round {
  def fromLines(input: Seq[String]): Seq[Round] = input.map(fromString)

  private def fromString(line: String): Round = {
    val opponent = GameAction.fromChar(line.charAt(0))
    val myAction = GameAction.fromChar(line.charAt(2))
    
    Round(opponentAction = opponent, myAction = myAction)
  }
 }

object Day2 extends IOApp.Simple {
 val run = for {
   lines <- readLine("/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/src/resources/2022/input-day2.txt")
   rounds = Round.fromLines(lines)
   total = rounds.map(_.getScore).sum
   _ <- IO.println(s"total score $total")
 } yield ()
}
