package ru.spbau.jirkov.tralalang

import scala.collection.mutable

object Interpreter {
  def apply(startNode: AST): InterpreterState = {
    val state = new InterpreterState
    state.Nodes(startNode)

    state
  }
}

class InterpreterState {

  abstract class Value

  sealed case class D(value: Double) extends Value

  sealed case class I(value: Int) extends Value

  sealed case class B(value: Boolean) extends Value

  case object `_|_` extends Value

  case object U extends Value


  sealed case class Variable(name: String, var value: Value) {
    override def toString = s"$name -> $value"
  }

  sealed class Context {
    def set(name: String, value: Value) = {
      if (vars.contains(name)) vars(name).value = value
      else vars(name) = new Variable(name, value)
    }

    def get(name: String): Value = {
      if (vars.contains(name)) vars(name).value
      else `_|_`
    }

    val vars = mutable.HashMap[String, Variable]()
  }

  private val _contexts = mutable.Stack[Context](new Context)

  def getVar(name: String): Value = {
    _contexts.find(_.get(name) != `_|_`) match {
      case None => `_|_`
      case Some(ctx) => ctx.get(name)
    }
  }

  object Nodes extends MultiMethod[AST, Value]({
    U
  })

  Nodes defImpl {
    case Assignment(v, e) => val computedValue = Nodes(e); _contexts.top.set(v.name, computedValue); computedValue
    case Sequence(l, r) => Nodes(l); Nodes(r)
  }


  Nodes defImpl {
    case IntLiteral(i) => I(i)
    case DoubleLiteral(i) => D(i)
    case TrueLiteral => B(value = true)
    case FalseLiteral => B(value = false)
    case Reference(name) => getVar(name)
  }

  Nodes defImpl {
    case Plus(l, r) => (Nodes(l), Nodes(r)) match {
      case (I(l), I(r)) => I(l + r)
      case (D(l), D(r)) => D(l + r)
      case (I(l), D(r)) => D(l + r)
      case (D(l), I(r)) => D(l + r)
    }
    case Minus(l, r) => (Nodes(l), Nodes(r)) match {
      case (I(l), I(r)) => I(l - r)
      case (D(l), D(r)) => D(l - r)
      case (I(l), D(r)) => D(l - r)
      case (D(l), I(r)) => D(l - r)
    }
    case Times(l, r) => (Nodes(l), Nodes(r)) match {
      case (I(l), I(r)) => I(l * r)
      case (D(l), D(r)) => D(l * r)
      case (I(l), D(r)) => D(l * r)
      case (D(l), I(r)) => D(l * r)
    }
    case Divide(l, r) => (Nodes(l), Nodes(r)) match {
      case (I(l), I(r)) => I(l / r)
      case (D(l), D(r)) => D(l / r)
      case (I(l), D(r)) => D(l / r)
      case (D(l), I(r)) => D(l / r)
    }
  }


  def state: String =
    _contexts.map(_.vars).flatten.map(_._2.toString).fold("")(_ + _ + "\n").trim
}