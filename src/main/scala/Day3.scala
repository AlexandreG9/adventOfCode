import scala.io.Source

case class CountState(one: Int, zero: Int) {
  def +(state: CountState): CountState = CountState(one = one + state.one, zero = zero + state.zero)

  def getMostPresent(): String = if (one >= zero) "1" else "0"
  def getLeastPresent(): String = if (getMostPresent equals  "1") "0" else "1"
}

object Day3 extends App {

  val filename = "input-day3.txt"
  val input = Source.fromFile(filename).getLines.toList
  val size = input(0).length

  def inverse(maxSize: Int): List[List[String]] = {
    def loop(index: Int, acc: List[List[String]]): List[List[String]] =
      if (index >= maxSize) acc
      else loop(index + 1, acc :+ getColumn(input, index))

    loop(0, List.empty)
  }

  val prettyInput = inverse(size)

  val gammaRate = prettyInput.map(finalCountState)
  val epsilonRate = gammaRate.map(_.getLeastPresent())


  val firstValue = getResult(gammaRate.map(_.getMostPresent()).reduce((a, b) => a + b), epsilonRate.reduce((a, b) => a + b))
  println(firstValue)

  val oxygenRating = getRating(input, _.getMostPresent())
  val co2Rating =  getRating(input, _.getLeastPresent())

  println(oxygenRating * co2Rating)

  def getResult(gamma:String, epsilon:String): Int = {
    val trueGamma = Integer.parseInt(gamma,2)
    val trueEpsilon = Integer.parseInt(epsilon, 2)

    trueGamma * trueEpsilon
  }

  def getColumn(list: List[String], index: Int): List[String] = list.map(_.charAt(index).toString)

  def getIndex(list: List[String], index: Int): List[Int] = list.map(line => line.charAt(index))

  def finalCountState(list: List[String]): CountState = list.map(value => value match {
    case "1" => CountState(1, 0)
    case "0" => CountState(0, 1)
  }).foldLeft(CountState(0, 0))((old, newState) => old + newState)


  def getRating(initialListInput: List[String], transformCountStateToString: CountState => String): Int = {
    def getMostBitByIndex(listInput: List[String], index: Int): List[String] ={
      val mostBit = transformCountStateToString(finalCountState(getColumn(listInput, index)))
      listInput.filter(_.charAt(index).toString.equals(mostBit))
    }

    def loop(listInput: List[String], index: Int): String = {
      if(listInput.length <= 1) listInput.head
      else loop(getMostBitByIndex(listInput, index), index + 1)
    }

    Integer.parseInt(loop(initialListInput, 0), 2)
  }
}
