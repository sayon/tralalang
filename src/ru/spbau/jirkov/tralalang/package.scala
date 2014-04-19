package ru.spbau.jirkov


package object tralalang {

  class MultiMethod[A, R](default: => R) {
    private var handlers: List[PartialFunction[A, R]] = Nil

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

}
