trait Day {
  partOne()
  partTwo()

  def day: Int

  def partOne(): Unit
  def partTwo(): Unit

  def result(r: String): Unit =
    println(s"Day $day: $r")
}
