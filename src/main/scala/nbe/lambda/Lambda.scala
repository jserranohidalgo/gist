package org.hablapps.gist.nbe
package lambda

trait Lambda[P[E, T]]{

  def int[E](i: Int): P[E, Int]

  def add[E](i1: P[E, Int])(i2: P[E, Int]): P[E, Int]

  def vz[E, T]: P[(T, E), T]

  def vs[E, T1, T](a: P[E, T]): P[(T1, E), T]

  def lam[E, T1, T2](t: P[(T1, E), T2]): P[E, T1 => T2]

  def app[E, T1, T2](f: P[E, T1 => T2])(t1: P[E, T1]): P[E, T2]
}

object Lambda{

  // implicit val ShowSemI: SystemT[λ[T => String]] = semantics.ShowSem

  implicit val StdSem: Lambda[Function1] = semantics.Standard
}
