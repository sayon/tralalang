package ru.spbau.jirkov.tralalang

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}

class TralaParser extends JavaTokenParsers with PackratParsers {
  lazy val integer: PackratParser[IntLiteral] = """-?[0-9]+""".r ^^ (i => IntLiteral(i.toInt))

  lazy val real: PackratParser[DoubleLiteral] = """-?[0-9]+\.[0-9]*""".r ^^ (f => DoubleLiteral(f.toDouble))

  lazy val trueLiteral: PackratParser[Literal[Boolean]] = "true" ^^ (_ => TrueLiteral)
  lazy val falseLiteral: PackratParser[Literal[Boolean]] = "false" ^^ (_ => FalseLiteral)

  lazy val liter: PackratParser[Expression] = trueLiteral | falseLiteral | real | integer
  lazy val ref = ident ^^ (s => Reference(s))

  lazy val sum : PackratParser[Expression] = (sum ~ ("+" ~> fact)) ^^ {case x~y => Plus(x,y)} |
    (sum ~ ("-" ~> fact)) ^^ {case x~y => Minus(x,y)} | fact
  lazy val fact: PackratParser[Expression] = (sum ~ ("*" ~> liter)) ^^ {case x~y => Times(x,y)} |
    (sum ~ ("/" ~> liter)) ^^ {case x~y => Divide(x,y)} | "(" ~> sum <~ ")" | liter | ref



  lazy val expr: PackratParser[Expression] = (expr ~ ("&&" ~> sum)) ^^ {case x~y => And(x,y)} |
    (expr ~ ("||" ~> sum)) ^^ {case x~y => Or(x,y)} | sum

  lazy val assignment: PackratParser[Statement] = ref ~ (":=" ~> expr) ^^ {
    case varr ~ exprr => Assignment(varr, exprr)
  }

  lazy val block: PackratParser[Block] = "{" ~> statement <~ "}" ^^ Block

  lazy val statement: PackratParser[Statement] = block | sequence | assignment

  lazy val sequence: PackratParser[Sequence] = statement ~ (";" ~> statement )^^ {
    case l ~ r => Sequence(l, r)
  }

}
