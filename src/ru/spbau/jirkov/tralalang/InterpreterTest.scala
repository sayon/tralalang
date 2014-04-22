package ru.spbau.jirkov.tralalang

import org.junit._
import Assert._
class InterpreterTest {
  val parser = new TralaParser
  def launchStatus(program:String) : String = {
    new Interpreter( parser.parseAll(parser.statement, program).get ).state.vars.trim.replace("\n","$")
  }
  @Test
  def assignment() = {
    assertEquals("x -> I(4)", launchStatus("x := 4"))
    assertEquals("x -> _|_", launchStatus("x := y"))
    assertEquals("x -> I(7)", launchStatus("x := 4 + 10 - 7"))
    assertEquals("x -> I(33)", launchStatus("x := 4 * 10 - 7"))

  }

  @Test
  def block() = {
    assertEquals("x -> I(5)", launchStatus("x := 4; { x := 5; y := 4 }"))
  }

  @Test
  def tuple() = {
    assertEquals("y -> S(List(_|_, I(4), I(2), B(true)))", launchStatus("y := [x, 4, 2, true]"))
  }

  @Test
  def tupleAccess() = {
    assertEquals("res -> I(2)", launchStatus("res := [x, 4, 2, true] -> 2"))
    assertEquals(
      """y -> B(true)$x -> S(List(I(4), B(true), I(2)))""", launchStatus("x := [4, true, 2] ; y := x -> 1"))
  }

}
