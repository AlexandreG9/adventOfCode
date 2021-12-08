import scala.io.Source

case class Point(x: Int, y: Int)

object Day5 extends App {
  val filename = "input-day5.txt"
  val input = Source.fromFile(filename).getLines.toList

  // Sort
  val points = getPoints(input).map({
    case (pA, pB) if pA.x > pB.x => (pB, pA)
    case (pA, pB) if pA.x == pB.x && pA.y > pB.y => (pB, pA)
    case tuple => tuple
  })
  val maxPoint = getMaxXY(points.flatMap(t => List(t._1, t._2)))

  val totalPointsCount = points.flatMap(t => getPointsFromVector(t._1, t._2))
    .foldLeft(Map.empty[Point, Int]) {
      (map, point) => map + (point -> (map.getOrElse(point, 0) + 1))
    }

  println(totalPointsCount.count(pred => pred._2 > 1))


  // Read input
  def getPoints(input: List[String]): List[(Point, Point)] = {
    input.map(row => {
      val points = row.split(" -> ")
      stringToPoint(points(0)) -> stringToPoint(points(1))
    })
  }

  def stringToPoint(string: String): Point = {
    val coord = string.split(",")
    Point(coord(0).toInt, coord(1).toInt)
  }

  def getMaxXY(points: List[Point]): Point = {
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max

    Point(maxX, maxY)
  }

  // Calculate points from vector
  def getPointsFromVector(orig: Point, dest: Point): List[Point] = {
    // println(s"points from vector $orig $dest")
    val useFunction = if (orig.x == dest.x) {
      println(s"vertical $orig $dest")
      verticalCompute
    }
    else if (orig.y == dest.y) {
      println(s"horizontal $orig $dest")
      horizontalCompute
    }
    else if (orig.x < dest.x && orig.y < dest.y) {
      println(s"diag $orig $dest")
      diagonalDownCompute
    }
    else {
      println(s"diag $orig $dest")
      diagonalUpCompute
    }

    def loop(lastPoint: Point, acc: List[Point], func: (Int, Int) => Point): List[Point] = {
      // calculate point
      val currentPoint = func(lastPoint.x, lastPoint.y)
      if (currentPoint.equals(dest)) acc :+ currentPoint
      else loop(currentPoint, acc :+ currentPoint, func)
    }

    if (orig.equals(dest)) List(orig) // Same point
    else loop(orig, List(orig), useFunction) // Horizontal / Vertical / Diagonal
  }

  private def horizontalCompute: (Int, Int) => Point = (x: Int, y: Int) => Point(x + 1, y)

  private def verticalCompute: (Int, Int) => Point = (x: Int, y: Int) => Point(x, y + 1)

  private def diagonalDownCompute: (Int, Int) => Point = (x: Int, y: Int) => Point(x + 1, y + 1)

  private def diagonalUpCompute: (Int, Int) => Point = (x: Int, y: Int) => Point(x + 1, y - 1)
}
