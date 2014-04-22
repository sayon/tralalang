package ru.spbau.jirkov.tralalang

import scala.collection.mutable

class Interpreter(startNode: AST) {

  abstract class Value

  sealed case class D(value: Double) extends Value

  sealed case class I(value: Long) extends Value

  sealed case class B(value: Boolean) extends Value

  sealed case class S(contents:List[Value]) extends Value

  case object `_|_` extends Value

  case object U extends Value


  class InterpreterException extends Throwable
  class TypeException extends InterpreterException

  object Handler extends MultiMethodWithState[AST, Value, InterpreterState]({
    throw new NotImplementedError("Unsupported node type!")
    U
  }, new InterpreterState)

  Handler defImpl {
    case Assignment(v, e) => val computedValue = Handler(e); Handler.state.setVar(v.name, computedValue); computedValue
    case Sequence(l,r ) => Handler(l); Handler(r)
    case Block(s) => Handler.state.pushContext(); val r = Handler(s); Handler.state.popContext(); r
  }


  Handler defImpl {
    case IntLiteral(i) => I(i)
    case DoubleLiteral(i) => D(i)
    case TrueLiteral => B(value = true)
    case FalseLiteral => B(value = false)
    case Reference(name) => Handler.state.getVar(name)
    case Tuple(contents) => S(contents.map(e => Handler(e)))
    case Skip => U

    case TupleAccess(t, i) => (Handler(t), Handler(i)) match {
      case (S(lst), I(i))=>  lst(i.toInt)
      case _ => throw new TypeException
    }
    case TupleStore(t, i, v) => (Handler(t),Handler(i),Handler(v)) match {
      case (s@S(t), I(i), v ) => S(t.updated(i.toInt, v))
    }

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

  protected object Util {
    def normalize(l:Value, r:Value) : Either[(Double,Double),(Long,Long)] = (l,r) match {
      case (D(l), D(r)) => Left((l.toDouble,r.toDouble))
      case (D(l), I(r)) => Left((l.toDouble, r.toDouble))
      case (I(l), D(r)) => Left((l.toDouble, r.toDouble))
      case (I(l), I(r)) => Right((l, r))
    }
    def compare(l:Value, r:Value) : Int = normalize(l,r) match {
      case Right((l,r)) => l compareTo r
      case Left((l,r)) => l compareTo r
    }
  }
  Handler defImpl {
    case Less(l,r) => B(Util.compare(Handler(l),Handler(r)) == -1)
    case LessOrEquals(l,r) => B(Util.compare(Handler(l),Handler(r)) <= 0)
    case Equals(l,r) => B(Util.compare(Handler(l),Handler(r)) == 0)
    case NotEquals(l,r) => B(Util.compare(Handler(l),Handler(r)) != 0)
    case GreaterOrEquals(l,r) => B(Util.compare(Handler(l),Handler(r)) >= 0)
    case Greater(l,r) => B(Util.compare(Handler(l),Handler(r)) == 1)
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

    def pushContext(): Unit = _contexts.push(new Context)

    def popContext(): Unit = _contexts.pop()

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

  def state = Handler.state

  Handler(startNode)
}