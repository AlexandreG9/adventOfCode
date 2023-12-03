package year2023

object Day2 extends App {
  val path = "/Users/alexandre/IdeaProjects/adventOfCode/2023/input-day2.txt"

  val maxColor = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )

  case class Round(red: Int, green: Int, blue: Int)

  case class MinimumSetOfCube(red: Int, green: Int, blue: Int)

  case class Game(gameId: Int, rounds: List[Round], isValid: Boolean)

  def openFile(path: String): List[String] = {
    val bufferedSource = io.Source.fromFile(path)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  def getGameId(line: String): Option[Int] = {
    val regexGameGroupId = """Game (\d+):""".r
    regexGameGroupId.findFirstMatchIn(line) match {
      case Some(matched) =>
        Some(matched.group(1).toInt)
      case None => None
    }
  }

  def getGame(line: String): Game = {
    val regex = """: (.+)""".r
    regex.findFirstMatchIn(line) match {
      case Some(matched) =>
        val rounds = matched.group(1).split(";").toList.map(_.trim)
          .flatMap(getRound(_))
        val gameId = getGameId(line).get
        val isValid = rounds.forall(round => {
          round.red <= maxColor("red") &&
            round.green <= maxColor("green") &&
            round.blue <= maxColor("blue")
        })

        Game(gameId, rounds, isValid)
      case None => Game(0, List(), false)
    }
  }

  def getRound(line: String): Option[Round] = {
    val regexBlue = """\s?(\d+) blue""".r
    val regexGreen = """\s?(\d+) green""".r
    val regexRed = """\s?(\d+) red""".r

    val blue = regexBlue.findFirstMatchIn(line) match {
      case Some(matched) =>
        matched.group(1).toInt
      case None => 0
    }

    val green = regexGreen.findFirstMatchIn(line) match {
      case Some(matched) =>
        matched.group(1).toInt
      case None => 0
    }

    val red = regexRed.findFirstMatchIn(line) match {
      case Some(matched) =>
        matched.group(1).toInt
      case None => 0
    }

    if (blue + green + red == 0) None
    else Some(Round(red, green, blue))
  }

  def getMinimumSetOfCube(game: Game): MinimumSetOfCube = {
    val rounds = game.rounds

    val red = rounds.map(_.red).max
    val green = rounds.map(_.green).max
    val blue = rounds.map(_.blue).max

    MinimumSetOfCube(red, green, blue)
  }


  val lines = openFile(path)
  val games = lines.map(getGame)
  val validGames = games.filter(_.isValid)
  val sumOfIds = validGames.map(_.gameId).sum


  val power = games.map(getMinimumSetOfCube)
    .map(set => set.red * set.green * set.blue)
    .sum

  println(sumOfIds)
  println(power)
}
