package ru.spbau.jirkov.tralalang

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}

class TralaParser extends JavaTokenParsers with PackratParsers {
  lazy val integer: PackratParser[IntLiteral] = """-?[0-9]+""".r ^^ (i => IntLiteral(i.toInt))

  lazy val real: PackratParser[DoubleLiteral] = """-?[0-9]+\.[0-9]*""".r ^^ (f => DoubleLiteral(f.toDouble))

  lazy val trueLiteral: PackratParser[Literal[Boolean]] = "true" ^^ (_ => TrueLiteral)
  lazy val falseLiteral: PackratParser[Literal[Boolean]] = "false" ^^ (_ => FalseLiteral)

  lazy val liter: PackratParser[Literal[_]] = trueLiteral | falseLiteral | real | integer
  lazy val ref = ident ^^ Reference

  lazy val expr: PackratParser[Expression] = expr ~ ("->" ~> exprB) ^^ { case t ~ i => TupleAccess(t,i) } | exprB

  lazy val exprB:PackratParser[Expression] = (exprB ~ ("&&" ~> comp)) ^^ {case x~y => And(x,y)} |
    (exprB ~ ("||" ~> comp)) ^^ {case x~y => Or(x,y)} |  comp

  lazy val comp: PackratParser[Expression] = comp ~ ("<" ~> sum) ^^ { case x~y => Less(x,y) } |
    comp ~ (">" ~> sum) ^^ { case x~y => Greater(x,y) } |
    comp ~ ("<=" ~> sum) ^^ { case x~y => LessOrEquals(x,y) } |
    comp ~ (">=" ~> sum) ^^ { case x~y => GreaterOrEquals(x,y) } |
    comp ~ ("==" ~> sum) ^^ { case x~y => Equals(x,y) } |
    comp ~ ("!=" ~> sum) ^^ { case x~y => NotEquals(x,y) } | sum

  lazy val sum : PackratParser[Expression] = (sum ~ ("+" ~> fact)) ^^ {case x~y => Plus(x,y)} |
    (sum ~ ("-" ~> fact)) ^^ {case x~y => Minus(x,y)} | fact

  lazy val fact: PackratParser[Expression] = (fact ~ ("*" ~> atom)) ^^ {case x~y => Times(x,y)} |
    (fact ~ ("/" ~> atom)) ^^ {case x~y => Divide(x,y)} | atom

  lazy val atom: PackratParser[Expression] = "(" ~> expr <~ ")" | liter | ref | tuple


  lazy val tuple: PackratParser[Tuple] = "[" ~> repsep(expr, ",") <~ "]" ^^ Tuple

  lazy val assignment: PackratParser[Statement] = ref ~ (":=" ~> expr) ^^ {
    case varr ~ exprr => Assignment(varr, exprr)
  }

  lazy val block: PackratParser[Block] = "{" ~> statement <~ "}" ^^ Block

  lazy val statement: PackratParser[Statement] =  block | sequence | functionDef | tupleStore | assignment | expr

  lazy val tupleStore: PackratParser[TupleStore] = expr ~ ("!!" ~> expr <~ ":=") ~ expr ^^ {
    case t ~ i ~ v => TupleStore(t,i,v)
  }

  lazy val sequence: PackratParser[Sequence] = statement ~ (";" ~> statement )^^ {
    case l ~ r => Sequence(l, r)
  }

  lazy val argDef: PackratParser[(Reference, Literal[_])] = ref ~ ("@" ~> liter) ^^ { case r ~ l => (r,l) }
  lazy val argList:PackratParser[ArgList] = "(" ~> repsep( ref, ",") <~ ")" ^^ ArgList

  lazy val defArgList:PackratParser[DefArgList] = ("(" ~> repsep( argDef, ",") <~ ")" ) ^^ DefArgList

  lazy val functionDef: PackratParser[FunctionDef] = "fun" ~> ident ~ argList ~ defArgList.? ~ block ^^ {
    case name ~ args ~ defs ~ body => FunctionDef(name, args, defs, body)
  }
}
