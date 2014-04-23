package ru.spbau.jirkov.tralalang

import org.junit._
import Assert._
class InterpreterTest {
  val parser = new TralaParser

  def launchStatus(program: String): String = {
    new Interpreter(parser.parseAll(parser.statement, program).get).state.vars.trim.replace("\n", "$")
  }

  @Test
  def assignment() = {
    assertEquals("x -> I(4)$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(x),IntLiteral(4))))",
      launchStatus("x := 4"))
    assertEquals("x -> _|_$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(x),Reference(y))))",
      launchStatus("x := y"))
    assertEquals("x -> I(7)$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(x),Minus(Plus(IntLiteral(4),IntLiteral(10)),IntLiteral(7)))))",
      launchStatus("x := 4 + 10 - 7"))
    assertEquals("x -> I(33)$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(x),Minus(Times(IntLiteral(4),IntLiteral(10)),IntLiteral(7)))))",
      launchStatus("x := 4 * 10 - 7"))

  }

  @Test
  def block() = {
    assertEquals("x -> I(5)$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(Assignment(Reference(x),IntLiteral(4)),Block(Sequence(Assignment(Reference(x),IntLiteral(5)),Assignment(Reference(y),IntLiteral(4)))))))",
      launchStatus("x := 4; { x := 5; y := 4 }"))
  }

  @Test
  def tuple() = {
    assertEquals("y -> S(List(_|_, I(4), I(2), B(true)))$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(y),Tuple(List(Reference(x), IntLiteral(4), IntLiteral(2), TrueLiteral)))))",
      launchStatus("y := [x, 4, 2, true]"))
  }

  @Test
  def tupleAccess() = {
    assertEquals("program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(res),TupleAccess(Tuple(List(Reference(x), IntLiteral(4), IntLiteral(2), TrueLiteral)),IntLiteral(2)))))$res -> I(2)",
      launchStatus("res := [x, 4, 2, true] -> 2"))
    assertEquals(
      """y -> B(true)$x -> S(List(I(4), B(true), I(2)))$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(Assignment(Reference(x),Tuple(List(IntLiteral(4), TrueLiteral, IntLiteral(2)))),Assignment(Reference(y),TupleAccess(Reference(x),IntLiteral(1))))))""",
      launchStatus("x := [4, true, 2] ; y := x -> 1"))
  }

  @Test
  def tupleStore() = {
    assertEquals("x -> S(List(I(666), I(1), I(2), I(3)))$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(Assignment(Reference(x),Tuple(List(IntLiteral(0), IntLiteral(1), IntLiteral(2), IntLiteral(3)))),Assignment(Reference(x),TupleStore(Reference(x),IntLiteral(0),Plus(IntLiteral(665),IntLiteral(1)))))))", launchStatus("x := [0,1,2,3];  x := x !! 0 := (665 + 1)"))
  }

  @Test
  def compare() = {
    assertEquals("", "")
  }

  @Test
  def skip() = {
    assertEquals("x -> I(2)$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(x),Block(Sequence(IntLiteral(1),IntLiteral(2))))))",
      launchStatus("x := { 1; 2 }"))
  }

  @Test
  def functionDef() = {
    assertEquals("f -> F(FunctionDef(f,ArgList(List(Reference(x), Reference(y))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Plus(Reference(x),Reference(y)),Reference(z)))))$program -> F(FunctionDef(program,ArgList(List()),None,FunctionDef(f,ArgList(List(Reference(x), Reference(y))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Plus(Reference(x),Reference(y)),Reference(z))))))", launchStatus("fun f(x,y)(z@4){ x + y + z }"))
  }

  @Test
  def function1() = {
    assertEquals("""y -> I(5)$f -> F(FunctionDef(f,ArgList(List(Reference(x))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Reference(x),Reference(z)))))$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(FunctionDef(f,ArgList(List(Reference(x))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Reference(x),Reference(z)))),Assignment(Reference(y),FunctionCall(f,List(IntLiteral(1)))))))""", launchStatus(
      """
        |
        |fun f(x)(z@4) { x + z };
        |y := f(1)
      """.stripMargin))
    assertEquals("""y -> I(18)$f -> F(FunctionDef(f,ArgList(List(Reference(x), Reference(y), Reference(l))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Plus(Plus(Reference(x),Reference(y)),Reference(l)),Reference(z)))))$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(FunctionDef(f,ArgList(List(Reference(x), Reference(y), Reference(l))),Some(DefArgList(List((Reference(z),IntLiteral(4))))),Block(Plus(Plus(Plus(Reference(x),Reference(y)),Reference(l)),Reference(z)))),Sequence(Assignment(Reference(y),FunctionCall(f,List(Times(IntLiteral(1),IntLiteral(9)), IntLiteral(2), IntLiteral(3)))),Sequence(PrintLn(List(Reference(y))),PrintLn(List(Reference(f))))))))""", launchStatus(
      """
        |
        |fun f(x, y, l)(z@4) { x + y + l + z };
        |y := f(1 * 9, 2, 3);
        |println(y);
        |println(f)
      """.stripMargin))

  }

  @Test
  def ifThenElse() = {
    assertEquals("r -> I(1)$program -> F(FunctionDef(program,ArgList(List()),None,Assignment(Reference(r),IfThenElse(Greater(IntLiteral(4),IntLiteral(3)),IntLiteral(1),IntLiteral(0)))))",
    launchStatus(
      """
        |r := if (4 > 3) 1 else 0
      """.stripMargin)
    )
  }

  @Test
  def whil() = {
    assertEquals("x -> I(3)$r -> I(3)$program -> F(FunctionDef(program,ArgList(List()),None,Sequence(Assignment(Reference(x),IntLiteral(1)),Assignment(Reference(r),While(Less(Reference(x),IntLiteral(3)),Block(Assignment(Reference(x),Plus(Reference(x),IntLiteral(1)))))))))",
    launchStatus(
      """
        |x := 1;
        |r := while ( x < 3 ) { x:= x + 1 }
      """.stripMargin))
  }
}
