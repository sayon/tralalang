package ru.spbau.jirkov.tralalang

sealed trait AST
sealed trait Statement extends AST

sealed case class Sequence(before: Statement, after:Statement) extends Statement
sealed case class Assignment(v:Reference, e:Expression) extends Statement
sealed case class Scope(s:Statement) extends Statement

sealed trait Expression extends AST

sealed case class Reference(name: String) extends Expression

sealed abstract class Literal[T](val value:T) extends Expression

sealed case class DoubleLiteral(v:Double) extends Literal[Double](v)
sealed case class IntLiteral(v:Integer) extends Literal[Int](v)
sealed case class StringLiteral(v:String) extends Literal[String](v)
case object TrueLiteral extends Literal[Boolean](true)
case object FalseLiteral extends Literal[Boolean](false)


sealed abstract class Binary(left:Expression, right:Expression) extends Expression

sealed case class Plus(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Minus(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Times(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Divide(left: Expression, right:Expression) extends Binary(left,right)
sealed case class Or(left: Expression, right:Expression) extends Binary(left,right)
sealed case class And(left: Expression, right:Expression) extends Binary(left,right)


