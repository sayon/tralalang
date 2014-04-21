package ru.spbau.jirkov.tralalang

sealed trait AST
sealed trait Statement extends AST

sealed case class Sequence(before: Statement, after:Statement) extends Statement
sealed case class Block(contents:Statement) extends Statement
sealed case class Assignment(v:Reference, e:Expression) extends Statement
sealed case class Scope(s:Statement) extends Statement

sealed trait Expression extends Statement

sealed case class Reference(name: String) extends Expression

sealed abstract class Literal[+T](val value:T) extends Expression

sealed case class DoubleLiteral(v:Double) extends Literal[Double](v)
sealed case class IntLiteral(v:Long) extends Literal[Long](v)
sealed case class StringLiteral(v:String) extends Literal[String](v)
case object TrueLiteral extends Literal[Boolean](true)
case object FalseLiteral extends Literal[Boolean](false)

sealed case class Tuple(contents:List[Expression]) extends Expression

sealed abstract class Binary(left:Expression, right:Expression) extends Expression

sealed case class Plus(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Minus(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Times(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Divide(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Or(left: Expression, right:Expression) extends Binary(left,right)
sealed case class And(left: Expression, right:Expression) extends Binary(left,right)

sealed case class ArgList(args:List[Reference]) extends AST
sealed case class DefArgList(args:List[(Reference,Literal[_])]) extends AST

sealed case class FunctionDef(name: String, args:ArgList, defaults: Option[DefArgList], body:Block) extends Statement
