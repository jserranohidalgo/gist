package org.hablapps.gist.nbe
package systemT
package semantics

sealed abstract class NonStd[_]
case class IntSem(i: Int) extends NonStd[Int]
case class FunSem[T1, T2](
  term: Term[T1 => T2],
  fun: NonStd[T1] => NonStd[T2]) extends NonStd[T1 => T2]

object NonStd{

  def reify[T]: NonStd[T] => Term[T] = {
    case IntSem(0) => Term.Sem.zero
    case IntSem(n) => Term.Sem.app(Term.Sem.succ, reify(IntSem(n-1)))
    case FunSem(t, _) => t
  }

  implicit object Sem extends SystemT[NonStd]{

    def K[T1, T2]: NonStd[T1 => T2 => T1] =
      FunSem(Term.Sem.K[T1, T2],
        (p: NonStd[T1]) =>
          FunSem(Term.Sem.app(Term.Sem.K[T1, T2], reify(p)),
            (q: NonStd[T2]) =>
              p))

    def S[T1, T2, T3]: NonStd[(T1 => T2 => T3) => (T1 => T2) => (T1 => T3)] =
      FunSem(Term.Sem.S[T1, T2, T3],
        (p: NonStd[T1 => T2 => T3]) =>
          FunSem(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)),
            (q: NonStd[T1 => T2]) =>
              FunSem(Term.Sem.app(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)), reify(q)),
                (r: NonStd[T1]) =>
                  app(app(p,r), app(q,r)))))

    def app[T1, T2](t1t2: NonStd[T1 => T2], t1: NonStd[T1]): NonStd[T2] =
      t1t2 match {
        case FunSem(_, f) => f(t1)
      }

    def zero: NonStd[Int] =
      IntSem(0)

    def succ: NonStd[Int => Int] =
      FunSem(Term.Sem.succ, {
        case IntSem(n) => IntSem(n + 1)
      })

    def rec[T]: NonStd[T => (Int => T => T) => (Int => T)] =
      FunSem(Term.Sem.rec,
        (t: NonStd[T]) =>
          FunSem(Term.Sem.app(Term.Sem.rec, reify(t)),
            (f: NonStd[Int => T => T]) =>
              FunSem(Term.Sem.app(Term.Sem.app(Term.Sem.rec[T], reify(t)), reify(f)), {
                case IntSem(0) => t
                case IntSem(n) => app(app(f, IntSem(n-1)), app(app(app(rec[T], t), f), IntSem(n-1)))
              })))
  }
}
