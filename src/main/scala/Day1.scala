import scala.io.Source

class Day1 extends Day {
  def day = 1

  def readNumbers(): Seq[Int] = {
    Source
      .fromResource("Day1.txt")
      .getLines()
      .map(_.toInt)
      .toList
  }

  override def partOne(): Unit = {
    // Read all numbers
    val numbers: Seq[Int] = readNumbers()

    // Product of two numbers
    // adding up to 2020
    val results: Seq[Int] = for(
      x <- numbers;
      y <- numbers;
      if x + y == 2020
    ) yield x * y

    result(results.head.toString)
  }

  override def partTwo(): Unit = {
    // Read all numbers
    val numbers: Seq[Int] = readNumbers()

    // Product of three numbers
    // adding up to 2020
    val results: Seq[Int] = for(
      x <- numbers;
      y <- numbers;
      z <- numbers;
      if x + y + z == 2020
    ) yield x * y * z

    result(results.head.toString)
  }
}
