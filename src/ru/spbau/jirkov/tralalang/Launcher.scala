package ru.spbau.jirkov.tralalang

object Launcher extends App {
  val program = """
    |fib_count := 6;
    |x1 := 1;
    |x2 := 1;
    |println(x1);
    |println(x2);
    |cur := 0;
    |last_cur := while (cur < fib_count - 2) {
    |   if (cur % 2 == 0) {
    |     x1 := x1 + x2;
    |     println(x1)
    |     }
    |     else {
    |        x2 := x2 + x1;
    |        println(x2)
    |        };
    |   cur := cur + 1
    |};
    |println( "hey, ", last_cur )
  """.stripMargin

  val p = new TralaParser
  val parseResult = p.parseAll(p.statement, program)
  parseResult match {
    case p.Error(str, i) =>
      println(s"Error! ${ProgramUtil.errorMessage(program, i.pos.line, i.pos.column)}")
      println(str)
    case p.Failure(str, i) =>
      println(s"Failure! ${ProgramUtil.errorMessage(program , i.pos.line, i.pos.column)}")
      println(str)
    case p.Success(result:Expression,_) =>
      new Interpreter(result)
      println("<end>")
  }

}

object ProgramUtil {
  def annotateWithLines(str:String):Array[String] =
    str.split(Array('\r','\n')).zipWithIndex.map(p => p._2.formatted("% 4d  ") + p._1 + "\n")

  def insertMarker(marker:String, strings:Array[String], line:Int, col: Int): Array[String]= {
    val newone = strings(line).splitAt(col)
    val withMarker = newone._1 + marker +newone._2
    strings.update(line, withMarker)
    strings
  }

  def errorMessage(program:String, line:Int, col:Int):String = {
    val lines = insertMarker("_________", annotateWithLines(program), line, col)
      lines.reduceLeftOption(_ + _).getOrElse(lines(0))
  }

}
