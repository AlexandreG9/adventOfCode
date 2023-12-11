package year2023

import cats.effect.{IO, IOApp}
import year2022.utils.FileUtils.readLine

import scala.annotation.tailrec

object Day8 extends IOApp.Simple {
  val path = "/Users/alexandreguiheneuf/IdeaProjects/adventOfCode/2023/input-day8.txt"

  case class Node(name: String, left: String, right: String)

  def traverseNodeStepsCount(nodeMap: Map[String, Node], instructionList: List[String]): Int = {
    @tailrec
    def loop(acc: Int, node: Node, instructions: List[String]): Int = {
      if (node.name == "ZZZ") acc
      else {
        val nextInstructions = if (instructions.isEmpty) instructionList else instructions
        val nextNode = if (nextInstructions.head == "L") node.left else node.right
        loop(acc + 1, nodeMap(nextNode), nextInstructions.tail)
      }
    }

    loop(0, nodeMap("AAA"), instructionList)
  }

  def parseNodes(lines: List[String]): List[Node] = {
    val regex = """\w{3}""".r

    lines.flatMap(line => {
      val matches = regex.findAllMatchIn(line).toList.map(m => m.group(0))

      if (matches.size == 3) Some(Node(matches.head, matches(1), matches(2)))
      else None
    })
  }

  override def run: IO[Unit] = for {
    lines <- readLine(path)
    nodeMap = parseNodes(lines.toList).groupBy(_.name).map(elem => elem._1 -> elem._2.head)
    instructionsList = lines.head.toCharArray.toList.map(_.toString)
    steps = traverseNodeStepsCount(nodeMap, instructionsList)
    _ <- IO(println(steps))
  } yield ()
}
