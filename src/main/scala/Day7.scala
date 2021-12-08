import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt, MINUTES}
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Failure, Success}

case class FuelConsumption(index: Int, fuel: Int)

object Day7 extends App {
  val filename = "input-day7.txt"
  val input = Source.fromFile(filename).getLines().toList.head

  val indexes = input.split(",").toList.map(_.toInt).sorted

  val allIndex = List.range(0, indexes.reverse.head)


  val start = System.currentTimeMillis()

  val future = Future.sequence(allIndex.map(computeFuel(indexes, _)))

  /*.onComplete(m => {
  m match {
    case Failure(exception) => println(s"fail $exception")
    case Success(value) => {
      val fuel = value.reduce((a, b) => if(a.fuel > b.fuel) b else a)
      println(fuel)
    }
  }
})*/


  val result = Await.result(future, Duration(10, MINUTES)).reduce((a, b) => if (a.fuel > b.fuel) b else a)

  val computeTime = System.currentTimeMillis() - start

  println(result)
  println(s"Computed in $computeTime ms")


  def computeFuelForOne(orig: Int, dest: Int): Int = Math.abs(dest - orig)

  def computeFuel(allCrab: List[Int], desiredPosition: Int): Future[FuelConsumption] = Future {
    val fuel = allCrab.map(computeFuelForOne(_, desiredPosition)).reduce(_ + _)
    FuelConsumption(desiredPosition, fuel)
  }

}