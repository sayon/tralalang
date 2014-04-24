package ru.spbau.jirkov.tralalang


object ASTPrinter extends MultiMethod[AST, String]({
  "<Unknown node>"
}) {
  private[this] var _indent = 0

  def newline = "\n" +  " " * _indent

  defImpl({
    case Sequence(fst, snd) => this(fst) + " ; " + newline + this(snd)
    case Block(s) => _indent += 1; val ret = "{" + newline + this(s); _indent -= 1; ret + newline + "}"
    case Assignment(v, e) => this(v) + " := " + this(e)
    case Skip => "skip"
    case DoubleLiteral(v) => v.toString
    case IntLiteral(i) => i.toString
    case StringLiteral(s) => s
    case TrueLiteral => "true"
    case FalseLiteral => "false"
    case StringLiteral(s) => "\"" + s + "\""
    case Reference(n) => n
    case Plus(l, r) => this(l) + " + " + this(r)
    case Minus(l, r) => this(l) + " + " + this(r)
    case Times(l, r) => this(l) + " + " + this(r)
    case Divide(l, r) => this(l) + " + " + this(r)
    case Or(l, r) => this(l) + " ||  " + this(r)
    case And(l, r) => this(l) + " && " + this(r)
    case Less(l, r) => this(l) + " < " + this(r)
    case LessOrEquals(l, r) => this(l) + " <= " + this(r)
    case Equals(l, r) => this(l) + " == " + this(r)
    case GreaterOrEquals(l, r) => this(l) + " >= " + this(r)
    case Greater(l, r) => this(l) + " > " + this(r)
    case NotEquals(l, r) => this(l) + " != " + this(r)
    case ArgList(l) => "(" +  l.map(_ name).reduceLeftOption[String]((x, y) => x + ", " + y).getOrElse({""}) + ")"
    case DefArgList(l) => "(" + l.map(x => x._1.name + "@" + this(x._2)).reduceLeftOption((x, y) => x + ", " + y).getOrElse({""}) + ")"
    case FunctionDef(name, args, defs, body) => val ds = defs match {
      case None => ""
      case Some(ds) => this(ds)
    }
      name + this(args) + ds + " " + this(body)
    case FunctionCall(name, args) => name + "(" +  args.map(x => this(x)).reduceLeftOption[String]((x, y) => x + ", " + y).getOrElse({""}) + ")"
    case PrintLn(args) => "println(" + args.map(this(_)).reduceLeftOption(_ + "," + _).getOrElse({""}) + ")"
  })
}
