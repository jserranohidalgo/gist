package org.hablapps.gist.nbe
package systemT

trait SystemT[P[_]]{

  def K[T1, T2]: P[T1 => (T2 => T1)]

  def S[T1, T2, T3]: P[(T1 => (T2 => T3)) => (T1 => T2) => (T1 => T3)]

  def app[T1, T2](f: P[T1 => T2], t1: P[T1]): P[T2]

  def zero: P[Int]

  def succ: P[Int => Int]

  def rec[T]: P[T => (Int => T => T) => (Int => T)]
}

object SystemT{

  implicit val ShowSemI: SystemT[λ[T => String]] = semantics.ShowSem

  implicit val StandardSem: SystemT[cats.Id] = semantics.Standard
}
