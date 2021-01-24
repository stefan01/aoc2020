package aoc2020

import org.scalatest._

class Day18Spec extends FlatSpec {
  val d18 = new Day18()
  val expr = "(4+(2*7)*4)+(6*9+8*4+7*3)*3*5"
 
  "readfile" should "read the file" in {
    val expressions = d18.readFile()
    assert(expressions.head === expr)
    assert(expressions.size === 373)
  }
 
  "evalPart1" should "return the right result" in {
    assert(d18.evalLeftToRight(expr) === 12555)
  }
 
  "evalPart2" should "return the right result" in {
    assert(d18.evalAddBeforeMul(expr) === 51570)
  }

  "evalParenthesis" should "return the right result" in {
    assert(d18.evalParenthesis(expr, d18.evalLeftToRight) === 12555)
    assert(d18.evalParenthesis(expr, d18.evalAddBeforeMul) === 51570)
  } 
}
