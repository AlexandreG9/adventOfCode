import scala.io.Source


object Day6 extends App {
  val filename = "input-day6.txt"
  val input = Source.fromFile(filename).getLines.toList.head.split(",")
    .map(_.toLong)
    .groupBy(identity)
    .map(p => p._1 -> p._2.size.toLong)

  val groups = List.range(0, 9).map(_.toLong).reverse.map(index => index -> input.get(index).getOrElse(0L).toLong)

  println(getFish(groups, 256L))

  def increaseDay(currentDay: List[(Long, Long)]): List[(Long, Long)] = {
    val nextDay = currentDay.foldLeft(List[(Long, Long)]())((list, c) => list :+ (c._1 - 1, c._2))
    val newFish = nextDay.collect({
      case (-1, value) => value
    }).head
    nextDay.flatMap(group => group match {
      case (-1, value) => List(8L -> value)
      case (6, value) => List(6L -> (value + newFish))
      case value => List(value)
    })
  }

  def getFish(initialDay: List[(Long, Long)], numberOfDay: Long): Long = {


    def loop(acc: List[(Long, Long)], day: Long): List[(Long, Long)] = {
      if (day > numberOfDay) acc
      else loop(increaseDay(acc), day + 1)
    }

    loop(initialDay, 1).map(_._2).sum
  }

}