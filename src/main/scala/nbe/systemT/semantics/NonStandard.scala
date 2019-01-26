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
    case IntSem(0) => Term.Sem.zero
    case IntSem(n) => Term.Sem.app(Term.Sem.succ, reify(IntSem(n-1)))
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
      FunSem(Term.Sem.S[T1, T2, T3],
        (p: NonStdSem[T1 => T2 => T3]) =>
          FunSem(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)),
            (q: NonStdSem[T1 => T2]) =>
              FunSem(Term.Sem.app(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)), reify(q)),
                (r: NonStdSem[T1]) =>
                  app(app(p,r), app(q,r)))))

    def app[T1, T2](t1t2: NonStdSem[T1 => T2], t1: NonStdSem[T1]): NonStdSem[T2] =
      t1t2 match {
        case FunSem(_, f) => f(t1)
      }

    def zero: NonStdSem[Int] =
      IntSem(0)

    def succ: NonStdSem[Int => Int] =
      FunSem(Term.Sem.succ, {
        case IntSem(n) => IntSem(n + 1)
      })

    def rec[T]: NonStdSem[T => (Int => T => T) => (Int => T)] =
      FunSem(Term.Sem.rec,
        (t: NonStdSem[T]) =>
          FunSem(Term.Sem.app(Term.Sem.rec, reify(t)),
            (f: NonStdSem[Int => T => T]) =>
              FunSem(Term.Sem.app(Term.Sem.app(Term.Sem.rec[T], reify(t)), reify(f)), {
                case IntSem(0) => t
                case IntSem(n) => app(app(f, IntSem(n-1)), app(app(app(rec[T], t), f), IntSem(n-1)))
              })))
      // t => f => ((i: Int) =>
      //   if (i == 0) t
      //   else f(i-1)(rec(t)(f)(i-1)))
  }
}
