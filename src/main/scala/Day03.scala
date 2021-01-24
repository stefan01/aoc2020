package aoc2020

import scala.io.Source

class Day03 extends Day {
  def day = 3

  sealed class MapTile

  case class OpenSquare() extends MapTile

  case class Tree() extends MapTile

  type World = Seq[Seq[MapTile]]

  def readFile(): World = {
    Source
      .fromResource("Day3.txt")
      .getLines()
      .map(line => line.toCharArray.map[MapTile] {
        case '.' => OpenSquare()
        case '#' => Tree()
        case _ => throw new RuntimeException("Error in Input Data")
      }.toSeq)
      .toSeq
  }

  override def partOne(): Unit = {
    val world = readFile()
    val slopePoints = LazyList.iterate((0, 0))(x => (x._1 + 3, x._2 + 1))
      .map(x => world.lift(x._1).flatMap(_.lift(x._2)))
      .takeWhile(_.isDefined)
      .map(_.get)

    result(slopePoints.mkString)
  }

  override def partTwo(): Unit = {
    result("Not implemented yet")
  }
}
