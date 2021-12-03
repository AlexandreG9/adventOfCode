import scala.io.Source

object Day1 extends App {
  val filename = "input-day1.txt"
  val input = Source.fromFile(filename).getLines.toList.map(_.toInt)

  val countMeasureIncrease = countIncrease(input)
  val groups = countIncrease(input.sliding(3).map(_.reduce((a, b) => a + b)).toList)

  println(countMeasureIncrease)
  println(groups)

  private def countIncrease(list: List[Int]): Int = {
    list.map(0 -> _)
      .reduce((a, b) =>
        if (a._2 < b._2) (a._1 + 1, b._2)
        else a._1 -> b._2)._1
  }
}