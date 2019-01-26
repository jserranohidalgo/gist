package org.hablapps.gist.nbe
package systemT
package semantics

object Standard extends SystemT[cats.Id]{

  def K[T1, T2]: T1 => T2 => T1 =
    t1 => _ => t1

  def S[T1, T2, T3]: (T1 => T2 => T3) => (T1 => T2) => (T1 => T3) =
    f1 => f2 => ((t1: T1) => f1(t1)(f2(t1)))

  def app[T1, T2](f: T1 => T2, t1: T1): T2 =
    f(t1)

  def zero: Int =
    0

  def succ: Int => Int =
    n => n + 1

  def rec[T]: T => (Int => T => T) => (Int => T) =
    t => f => ((i: Int) =>
      if (i == 0) t
      else f(i-1)(rec(t)(f)(i-1)))
}
