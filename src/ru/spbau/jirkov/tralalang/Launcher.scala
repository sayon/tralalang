package ru.spbau.jirkov.tralalang

object Launcher extends App {
  val program =
    """
      |x1 := 1;
      |x2 := 1;
      |println(x1);
      |println(x2);
      |cur := 0;
      |while (cur < 5) {
      |   x1 := x1 + x2;
      |   println(x1);
      |   x2 := x2 + x1;
      |   println(x2);
      |   cur := cur + 1
      |}
    """.stripMargin
  val p = new TralaParser
  val parseResult = p.parseAll(p.statement, program)
  parseResult match {
    case p.Error(str, i) =>
      println(s"Error! ${i.source}")
      println(str)
    case p.Failure(str, i) =>
      println(s"Failure! ${i.source}")
      println(str)
    case p.Success(result:Expression,_) =>
      new Interpreter(result)
      println("<end>")
  }
}
