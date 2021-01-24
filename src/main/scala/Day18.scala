package aoc2020

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

  def evalLeftToRight(expr: String): BigDecimal =
    if(expr.contains('(')) {
      evalParenthesis(expr, evalLeftToRight)
    } else {
      // Start from the end
      val idxPlus = expr.lastIndexOf('+')
      val idxMul = expr.lastIndexOf('*')

      if(idxPlus != -1 && idxPlus > idxMul)
        evalLeftToRight(expr.take(idxPlus)) + evalLeftToRight(expr.drop(idxPlus+1))
      else if(idxMul != -1 && idxMul > idxPlus)
        evalLeftToRight(expr.take(idxMul)) * evalLeftToRight(expr.drop(idxMul+1))
      else
        BigDecimal(expr.toDouble)
    }

  def evalAddBeforeMul(expr: String): BigDecimal =
    if(expr.contains('(')) {
      evalParenthesis(expr, evalAddBeforeMul)
    } else {
      val idxPlus = expr.indexOf('+')
      val idxMul = expr.indexOf('*')

      if(idxMul != -1)
        evalAddBeforeMul(expr.take(idxMul)) * evalAddBeforeMul(expr.drop(idxMul+1))
      else if(idxPlus != -1)
        evalAddBeforeMul(expr.take(idxPlus)) + evalAddBeforeMul(expr.drop(idxPlus+1))
      else
        BigDecimal(expr.toDouble)
    }

  override def partOne(): Unit = {
    val sum =
      readFile()
        .map(evalLeftToRight)
        .sum

    result(sum.toString())
  }

  override def partTwo(): Unit = {
    val sum =
      readFile()
        .map(evalAddBeforeMul)
        .sum

    result(sum.toString())
  }
}
