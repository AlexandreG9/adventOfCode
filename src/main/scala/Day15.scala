import scala.collection.mutable.CollisionProofHashMap.Node
import scala.io.Source

case class Path(points: List[Point], total: Int)



case class Graph(nodes: Node)

object Day15 extends App {
  // Input
  val filename = "input-day15.txt"
  val input = Source.fromFile(filename).getLines.toList.map(_.toList)

  // Transform to point of list
  val pointList = (0 to (input.size - 1)).foldLeft(Seq[Seq[Point]]())((list, row) => {
    val rowLine = input(row)
    val pointRow = (0 to (rowLine.size - 1)).foldLeft(Seq[Point]())((list, column) => {
      list :+ Point(x = column, y = row, value = rowLine(column).asDigit)
    })
    list :+ pointRow
  }).flatten.toList


  // Calculate path
  val firstPath = Path(points = List(pointList.head), total = pointList.head.value)

  val pp = getBetterPoint(orig = pointList.head, knownPoints = List(pointList.head), pointList.tail)

  println(stepForward(firstPath, pp.head))




  def stepForward(path: Path, lastPoint: Point): List[Path] = {
      getBetterPoint(lastPoint, path.points, pointList)
        .map(pp => Path(points = path.points :+ pp, total = path.total + pp.value))
  }

  def getBetterPoint(orig: Point, knownPoints: List[Point], allPoint: List[Point]): List[Point] = {
    val nearest = allPoint.filterNot(knownPoints.contains)
      .filter(p => Math.abs(orig.x - p.x) <= 1 && Math.abs(orig.y - p.y) <= 1)
    val minimalValue = nearest.map(_.value).min
    println(s"minimal value $minimalValue")
    nearest.filter(p => p.value == minimalValue)
  }

  def comparePoints(pA: Point, pB: Point): Point = if (pA.x >= pB.x && pA.y >= pB.y) pA else pB
}
