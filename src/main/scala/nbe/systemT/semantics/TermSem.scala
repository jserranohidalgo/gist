package org.hablapps.gist.nbe
package systemT
package semantics

trait Term[T]{
  def apply[P[_]](implicit S: SystemT[P]): P[T]
}

object Term{

  def norm[T]: Term[T] => Term[T] =
    t => NonStdSem.reify(t[NonStdSem])

  implicit object Sem extends SystemT[Term]{

    def K[T1, T2]: Term[T1 => T2 => T1] =
      ???

    def S[T1, T2, T3]: Term[(T1 => T2 => T3) => (T1 => T2) => (T1 => T3)] =
      ???

    def app[T1, T2](f: Term[T1 => T2], t1: Term[T1]): Term[T2] =
      ???

    def zero: Term[Int] =
      ???

    def succ: Term[Int => Int] =
      ???

    def rec[T]: Term[T => (Int => T => T) => (Int => T)] =
      ???
  }
}
