package ru.spbau.jirkov.tralalang

import org.junit._
import Assert._

class ParserTests {

  val p = new TralaParser

  def check[T](seed: String, estimated: String)(implicit symbol: p.Parser[T]) = {
    assertEquals(estimated, p.parseAll(symbol, seed).get.toString)
  }

  @Test
  def binop() = {
    implicit val sym = p.expr
    check("3 + 2", "Plus(IntLiteral(3),IntLiteral(2))")
    check("3 + 2.0", "Plus(IntLiteral(3),DoubleLiteral(2.0))")
    check("3.0 + 22 + s", "Plus(DoubleLiteral(3.0),Plus(IntLiteral(22),Reference(s)))")
    check("3 || 2.0", "Or(IntLiteral(3),DoubleLiteral(2.0))")
    check("3 && 2.0", "And(IntLiteral(3),DoubleLiteral(2.0))")
  }

  @Test
  def literal() = {
    implicit val sym = p.liter
    check("34", "IntLiteral(34)")
    check("1", "IntLiteral(1)")
    check("12.0", "DoubleLiteral(12.0)")
    check("true", "TrueLiteral")
    check("false", "FalseLiteral")
  }
}
