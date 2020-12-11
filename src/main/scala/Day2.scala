import scala.io.Source

class Day2 extends Day {
  def day = 2

  case class PasswordWithPolicy(min: Int, max: Int, char: Char, password: String)

  def parseLine(text: String): PasswordWithPolicy =
    PasswordWithPolicy(
      text.split("-")(0).toInt,
      text.split("[- ]")(1).toInt,
      text.split("[- :]")(2).toCharArray()(0),
      text.split("[- :]")(4)
    )


  def readPasswords(): List[PasswordWithPolicy] =
    Source
      .fromResource("Day2.txt")
      .getLines()
      .map(parseLine)
      .toList

  def passwordValidPartOne(passwordWithPolicy: PasswordWithPolicy): Boolean = {
    val count = passwordWithPolicy
      .password
      .toCharArray
      .count(_ == passwordWithPolicy.char)

    count >= passwordWithPolicy.min && count <= passwordWithPolicy.max
  }

  override def partOne(): Unit = {
    val validCount = readPasswords()
      .count(passwordValidPartOne)

    result(validCount.toString)
  }

  def passwordValidPartTwo(passwordWithPolicy: PasswordWithPolicy): Boolean = {
    val chars = passwordWithPolicy
      .password
      .toCharArray

    val char = passwordWithPolicy.char

    (chars(passwordWithPolicy.min - 1) == char) != (chars(passwordWithPolicy.max - 1) == char)
  }

  override def partTwo(): Unit = {
    val validCount = readPasswords()
      .count(passwordValidPartTwo)

    result(validCount.toString)
  }
}
