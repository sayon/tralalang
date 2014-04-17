package ru.spbau.jirkov.tralalang

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}

class TralaParser extends JavaTokenParsers with PackratParsers {
  lazy val integer: PackratParser[IntLiteral] = """-?[0-9]+""".r ^^ (i => IntLiteral(i.toInt))

  lazy val real: PackratParser[DoubleLiteral] = """-?[0-9]+\.[0-9]*""".r ^^ (f => DoubleLiteral(f.toDouble))

  lazy val trueLiteral: PackratParser[Literal[Boolean]] = "true" ^^ (_ => TrueLiteral)
  lazy val falseLiteral: PackratParser[Literal[Boolean]] = "false" ^^ (_ => FalseLiteral)

  lazy val liter: PackratParser[Expression] = trueLiteral | falseLiteral | real | integer
  lazy val ref = ident ^^ (s => Reference(s))

  lazy val binop: PackratParser[Binary] = expr ~ ("+" | "-" | "*" | "/" | "||" | "&&") ~ expr ^^ {
    case l ~ op ~ r => op match {
      case "+" => Plus(l, r)
      case "-" => Minus(l, r)
      case "*" => Times(l, r)
      case "/" => Divide(l, r)
      case "||" => Or(l, r)
      case "&&" => And(l, r)
    }
  }

  lazy val expr: PackratParser[Expression] = binop | liter | ref
  lazy val assignment: PackratParser[Statement] = (ref ~ ":=" ~ expr) ^^ {
    case r ~ ":=" ~ e => Assignment(r, e)
  }

  lazy val statement: PackratParser[Statement] = assignment

  lazy val statements: PackratParser[Sequence] = statement ~ ";" ~ statement ^^ {
    case l ~ _ ~ r => Sequence(l, r)
  }

}
