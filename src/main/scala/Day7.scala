import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, MINUTES}
import scala.concurrent.{Await, Future}
import scala.io.Source

case class FuelConsumption(index: Int, fuel: Int)

object Day7 extends App {
  val filename = "input-day7.txt"
  val input = Source.fromFile(filename).getLines().toList.head

  val indexes = input.split(",").toList.map(_.toInt).sorted

  val allIndex = List.range(0, indexes.reverse.head)

  val start = System.currentTimeMillis()

  val future = Future.sequence(allIndex.map(computeFuel(indexes, _)))


  val result = Await.result(future, Duration(10, MINUTES)).reduce((a, b) => if (a.fuel > b.fuel) b else a)

  val computeTime = System.currentTimeMillis() - start

  println(result)
  println(s"Computed in $computeTime ms") // Computed in 5610 ms


  def computeFuelForOne(orig: Int, dest: Int): Int = {
    val dist = Math.abs(dest - orig)

    val step = List.range(1, dist + 1)

    step.foldLeft(0)((a, b) => a + b)
  }

  def computeFuel(allCrab: List[Int], desiredPosition: Int): Future[FuelConsumption] = Future {
    val fuel = allCrab.map(computeFuelForOne(_, desiredPosition)).reduce(_ + _)
    FuelConsumption(desiredPosition, fuel)
  }

}