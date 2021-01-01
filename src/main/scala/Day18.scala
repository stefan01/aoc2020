import scala.io.Source

class Day18 extends Day {
  override def day: Int = 18

  def readFile(): Seq[String] = {
    Source
      .fromResource("Day18.txt")
      .getLines()
      .map(_.replace(" ", ""))
      .toSeq
  }

  def evalParenthesis(expr: String, eval: String => BigDecimal): BigDecimal = {
    // Find first '('
    val idxOpen = expr.indexOf('(')
    // Find last ')'
    val idxClose = expr.drop(idxOpen+1).scanLeft(1)((a, b) =>
      if (b == '(')
        a + 1
      else if (b == ')')
        a - 1
      else a
    ).indexOf(0) + idxOpen

    eval(expr.take(idxOpen) + eval(expr.substring(idxOpen+1, idxClose)) + expr.drop(idxClose+1))
  }

  def evalPart1(expr: String): BigDecimal =
    if(expr.contains('(')) {
      evalParenthesis(expr, evalPart1)
    } else {
      // Start from the end
      val idxPlus = expr.lastIndexOf('+')
      val idxMul = expr.lastIndexOf('*')

      if(idxPlus != -1 && idxPlus > idxMul)
        evalPart1(expr.take(idxPlus)) + evalPart1(expr.drop(idxPlus+1))
      else if(idxMul != -1 && idxMul > idxPlus)
        evalPart1(expr.take(idxMul)) * evalPart1(expr.drop(idxMul+1))
      else
        BigDecimal(expr.toDouble)
    }

  def evalPart2(expr: String): BigDecimal =
    if(expr.contains('(')) {
      evalParenthesis(expr, evalPart2)
    } else {
      val idxPlus = expr.indexOf('+')
      val idxMul = expr.indexOf('*')

      if(idxMul != -1)
        evalPart2(expr.take(idxMul)) * evalPart2(expr.drop(idxMul+1))
      else if(idxPlus != -1)
        evalPart2(expr.take(idxPlus)) + evalPart2(expr.drop(idxPlus+1))
      else
        BigDecimal(expr.toDouble)
    }


  override def partOne(): Unit = {
    val sum =
      readFile()
        .map(evalPart1)
        .sum

    println(sum)
  }

  override def partTwo(): Unit = {
    val sum =
      readFile()
        .map(evalPart2)
        .sum

    println(sum)
  }
}