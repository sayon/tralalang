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

  lazy val fact: PackratParser[Expression] = (fact ~ ("*" ~> tupleMutation)) ^^ {case x~y => Times(x,y)} |
    (fact ~ ("/" ~> tupleMutation)) ^^ {case x~y => Divide(x,y)} | tupleMutation

  lazy val tupleMutation : PackratParser[Expression] = tupleMutation ~ ("!!" ~> atom <~ ":=") ~ atom ^^ {
    case t ~ i ~ v => TupleStore(t,i,v)
  } | atom

  lazy val atom: PackratParser[Expression] = "(" ~> expr <~ ")" | liter | funCall | ref | tuple | statement

  lazy val tuple: PackratParser[Tuple] = "[" ~> repsep(expr, ",") <~ "]" ^^ Tuple

  lazy val assignment: PackratParser[Statement] = ref ~ (":=" ~> expr) ^^ {
    case varr ~ exprr => Assignment(varr, exprr)
  }

  lazy val block: PackratParser[Block] = "{" ~> statement <~ "}" ^^ Block

  lazy val statement: PackratParser[Expression] =  block | sequence | functionDef  | assignment | skip | expr

  lazy val skip : PackratParser[Skip.type] = "_" ^^ { _ => Skip }


  lazy val sequence: PackratParser[Sequence] = statement ~ (";" ~> statement )^^ {
    case l ~ r => Sequence(l, r)
  }

  lazy val funCall: PackratParser[FunctionCallOrPredef] = ident ~ ( "("~>repsep(expr, ",") <~")" ) ^^ defaultFunctions.orElse {
    case n ~ args => FunctionCall(n, args)
  }

  val defaultFunctions : PartialFunction[~[String, List[Expression]], PredefFunction] = {
    case "println" ~ args => PrintLn(args)
  }

  lazy val argDef: PackratParser[(Reference, Literal[_])] = ref ~ ("@" ~> liter) ^^ { case r ~ l => (r,l) }
  lazy val argList:PackratParser[ArgList] = "(" ~> repsep( ref, ",") <~ ")" ^^ ArgList

  lazy val defArgList:PackratParser[DefArgList] = ("(" ~> repsep( argDef, ",") <~ ")" ) ^^ DefArgList

  lazy val functionDef: PackratParser[FunctionDef] = "fun" ~> ident ~ argList ~ defArgList.? ~ statement ^^ {
    case name ~ args ~ defs ~ body => FunctionDef(name, args, defs, body)
  }
}
