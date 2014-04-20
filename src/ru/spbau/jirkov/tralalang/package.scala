package ru.spbau.jirkov


package object tralalang {

  class MultiMethod[A, R](default: => R) {
    protected var handlers: List[PartialFunction[A, R]] = Nil
    def apply(args: A): R = {
      handlers find {
        _.isDefinedAt(args)
      } map {
        _.apply(args)
      } getOrElse default
    }

    def defImpl(handler: PartialFunction[A, R]) = {
      handlers +:= handler
    }
  }

  class MultiMethodWithState[A,R,S](default: => R, val state: S) extends MultiMethod[A,R](default)

}
