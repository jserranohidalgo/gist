package org.hablapps.gist.nbe
package systemT
package semantics

sealed abstract class NonStdSem[_]
case class IntSem(i: Int) extends NonStdSem[Int]
case class FunSem[T1, T2](
  term: Term[T1 => T2],
  fun: NonStdSem[T1] => NonStdSem[T2]) extends NonStdSem[T1 => T2]

object NonStdSem{

  def reify[T]: NonStdSem[T] => Term[T] = {
    case IntSem(i) => ???
    case FunSem(t, _) => t
  }

  implicit object NonStdSemSystemT extends SystemT[NonStdSem]{

    def K[T1, T2]: NonStdSem[T1 => T2 => T1] =
      FunSem(Term.Sem.K[T1, T2],
        (p: NonStdSem[T1]) =>
          FunSem(Term.Sem.app(Term.Sem.K[T1, T2], reify(p)),
            (q: NonStdSem[T2]) =>
              p))

    def S[T1, T2, T3]: NonStdSem[(T1 => T2 => T3) => (T1 => T2) => (T1 => T3)] =
      ???

    def app[T1, T2](f: NonStdSem[T1 => T2], t1: NonStdSem[T1]): NonStdSem[T2] =
      ???

    def zero: NonStdSem[Int] =
      IntSem(0)

    def succ: NonStdSem[Int => Int] =
      ???

    def rec[T]: NonStdSem[T => (Int => T => T) => (Int => T)] =
      ???
      // t => f => ((i: Int) =>
      //   if (i == 0) t
      //   else f(i-1)(rec(t)(f)(i-1)))
  }
}
