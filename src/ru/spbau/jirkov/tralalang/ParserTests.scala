package ru.spbau.jirkov.tralalang

import org.junit._
import Assert._

class ParserTests {

  val p = new TralaParser

  def check[T](seed: String, estimated: String)(implicit symbol: p.Parser[T]) = {
    assertEquals(estimated, p.parseAll(symbol, seed).get.toString)
  }

  @Test
  def block() = {
    implicit val sym = p.statement
    check("x := 3 + 2; {x := 4}", "Sequence(Assignment(Reference(x),Plus(IntLiteral(3),IntLiteral(2))),Block(Assignment(Reference(x),IntLiteral(4))))")
  }

  @Test
  def binop() = {
    implicit val sym = p.expr
    check("3 + 2", "Plus(IntLiteral(3),IntLiteral(2))")
    check("3 + 2.0", "Plus(IntLiteral(3),DoubleLiteral(2.0))")
    check("3.0 + 22 + s", "Plus(Plus(DoubleLiteral(3.0),IntLiteral(22)),Reference(s))")
    check("3 || 2.0", "Or(IntLiteral(3),DoubleLiteral(2.0))")
    check("3 && 2.0", "And(IntLiteral(3),DoubleLiteral(2.0))")
  }

  @Test
  def literal() = {
    implicit val sym = p.expr
    check("34", "IntLiteral(34)")
    check("1", "IntLiteral(1)")
    check("12.0", "DoubleLiteral(12.0)")
    check("true", "TrueLiteral")
    check("false", "FalseLiteral")
  }

  @Test
  def assignment() = {
    implicit val sym = p.statement
    check("x := 4", "Assignment(Reference(x),IntLiteral(4))")
    check("x := 4 + 10 + x", "Assignment(Reference(x),Plus(Plus(IntLiteral(4),IntLiteral(10)),Reference(x)))")
  }

  @Test
  def precedence() = {
    implicit val sym = p.expr
    check("4 + 3 * 2 + 1", "Plus(Plus(IntLiteral(4),Times(IntLiteral(3),IntLiteral(2))),IntLiteral(1))")
  }

  @Test
  def functionDef() = {
    implicit val sym = p.statement
    check("fun myFun(firstArg, secondArg) { x := 4 + firstArg }", "FunctionDef(myFun,ArgList(List(Reference(firstArg), Reference(secondArg))),None,Block(Assignment(Reference(x),Plus(IntLiteral(4),Reference(firstArg)))))")
    check("fun myFun(){ x := 4 + firstArg }", "FunctionDef(myFun,ArgList(List()),None,Block(Assignment(Reference(x),Plus(IntLiteral(4),Reference(firstArg)))))")
    check("fun myFun(firstArg, secondArg)(z@4, q@6){ x := q + 4 + firstArg }", "FunctionDef(myFun,ArgList(List(Reference(firstArg), Reference(secondArg))),Some(DefArgList(List((Reference(z),IntLiteral(4)), (Reference(q),IntLiteral(6))))),Block(Assignment(Reference(x),Plus(Plus(Reference(q),IntLiteral(4)),Reference(firstArg)))))")
  }

  @Test
  def tuple() = {
    implicit val sym = p.statement
    check("x := 4; [x+ 4, y + 2, 9 +2 * 4, false ]", "Sequence(Assignment(Reference(x),IntLiteral(4)),Tuple(List(Plus(Reference(x),IntLiteral(4)), Plus(Reference(y),IntLiteral(2)), Plus(IntLiteral(9),Times(IntLiteral(2),IntLiteral(4))), FalseLiteral)))")

  }
}
