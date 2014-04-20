package ru.spbau.jirkov.tralalang

import org.junit._
import Assert._
class InterpreterTest {
  val parser = new TralaParser
  def launchStatus(program:String) : String = {
    new Interpreter( parser.parseAll(parser.assignment, program).get ).state.vars
  }
  @Test
  def assignment() {
    assertEquals("x -> I(4)", launchStatus("x := 4"))
    assertEquals("x -> _|_", launchStatus("x := y"))
    assertEquals("x -> I(7)", launchStatus("x := 4 + 10 - 7"))
    assertEquals("x -> I(33)", launchStatus("x := 4 * 10 - 7"))

  }
}
