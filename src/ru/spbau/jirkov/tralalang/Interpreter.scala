package ru.spbau.jirkov.tralalang

import scala.collection.mutable

class Interpreter(startNode: AST) {
  val state = new InterpreterState

  abstract class Value

  sealed case class D(value: Double) extends Value

  sealed case class I(value: Long) extends Value

  sealed case class B(value: Boolean) extends Value

  case object `_|_` extends Value

  case object U extends Value


  object Handler extends MultiMethod[AST, Value]({
    throw new NotImplementedError("Unsupported node type!")
    U
  })

  Handler defImpl {
    case Assignment(v, e) => val computedValue = Handler(e); state.setVar(v.name, computedValue); computedValue
    case Sequence(l, r) => Handler(l); Handler(r)
  }


  Handler defImpl {
    case IntLiteral(i) => I(i)
    case DoubleLiteral(i) => D(i)
    case TrueLiteral => B(value = true)
    case FalseLiteral => B(value = false)
    case Reference(name) => state.getVar(name)
  }

  Handler defImpl {
    case Plus(l, r) => (Handler(l), Handler(r)) match {
      case (I(l), I(r)) => I(l + r)
      case (D(l), D(r)) => D(l + r)
      case (I(l), D(r)) => D(l + r)
      case (D(l), I(r)) => D(l + r)
    }
    case Minus(l, r) => (Handler(l), Handler(r)) match {
      case (I(l), I(r)) => I(l - r)
      case (D(l), D(r)) => D(l - r)
      case (I(l), D(r)) => D(l - r)
      case (D(l), I(r)) => D(l - r)
    }
    case Times(l, r) => (Handler(l), Handler(r)) match {
      case (I(l), I(r)) => I(l * r)
      case (D(l), D(r)) => D(l * r)
      case (I(l), D(r)) => D(l * r)
      case (D(l), I(r)) => D(l * r)
    }
    case Divide(l, r) => (Handler(l), Handler(r)) match {
      case (I(l), I(r)) => I(l / r)
      case (D(l), D(r)) => D(l / r)
      case (I(l), D(r)) => D(l / r)
      case (D(l), I(r)) => D(l / r)
    }
  }


  class InterpreterState {

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

    def setVar(name: String, value: Value) = {
      _contexts.find(_.get(name) != `_|_`) match {
        case None => _contexts.top.set(name, value)
        case Some(ctx) => ctx.vars(name).value = value
      }
    }

    def vars: String =
      _contexts.map(_.vars).flatten.map(_._2.toString).fold("")(_ + _ + "\n").trim
  }


  Handler(startNode)
}