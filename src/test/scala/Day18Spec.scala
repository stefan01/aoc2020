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
 
  "evalLeftToRight" should "eval 1+2*3" in {
    assert(d18.evalLeftToRight("1+2*3") === 9)
  }
 
  it should "eval 1*2+3" in {
    assert(d18.evalLeftToRight("1*2+3") === 5)
  }
 
  it should "eval 1+(2*3)" in {
    assert(d18.evalLeftToRight("1+(2*3)") === 7)
  }
 
  it should "eval 1*(2+3)" in {
    assert(d18.evalLeftToRight("1*(2+3)") === 5)
  }
 
  it should "return the right result" in {
    assert(d18.evalLeftToRight(expr) === 12555)
  }

  "evalAddBeforeMul" should "eval 2*2+3" in {
    assert(d18.evalAddBeforeMul("2*2+3") === 10)
  }

  it should "eval (2*2)+3" in {
    assert(d18.evalAddBeforeMul("(2*2)+3") === 7)
  }

  it should "return the right result" in {
    assert(d18.evalAddBeforeMul(expr) === 51570)
  }

  "eval" should "handle empty expressions" in {
    assertThrows[IllegalArgumentException](d18.evalLeftToRight(""))
    assertThrows[IllegalArgumentException](d18.evalLeftToRight("()"))
    assertThrows[IllegalArgumentException](d18.evalAddBeforeMul(""))
    assertThrows[IllegalArgumentException](d18.evalAddBeforeMul("()"))
  }
 
  it should "handle unbalanced parenthis" in {
    assertThrows[IllegalArgumentException](d18.evalLeftToRight(")"))
    assertThrows[IndexOutOfBoundsException](d18.evalLeftToRight("("))
    assertThrows[IllegalArgumentException](d18.evalLeftToRight("()("))

    assertThrows[IllegalArgumentException](d18.evalAddBeforeMul(")"))
    assertThrows[IndexOutOfBoundsException](d18.evalAddBeforeMul("("))
    assertThrows[IllegalArgumentException](d18.evalAddBeforeMul("()("))
  }

  "evalParenthesis" should "return the right result" in {
    assert(d18.evalParenthesis(expr, d18.evalLeftToRight) === 12555)
    assert(d18.evalParenthesis(expr, d18.evalAddBeforeMul) === 51570)
  } 
}
