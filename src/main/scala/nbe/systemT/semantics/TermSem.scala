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

    def K[T1, T2] = new Term[T1 => T2 => T1]{
      def apply[P[_]](implicit S: SystemT[P]) = S.K
    }

    def S[T1, T2, T3] = new Term[(T1 => T2 => T3) => (T1 => T2) => (T1 => T3)]{
      def apply[P[_]](implicit S: SystemT[P]) = S.S
    }

    def app[T1, T2](f: Term[T1 => T2], t1: Term[T1]) = new Term[T2]{
      def apply[P[_]](implicit S: SystemT[P]) = S.app(f(S), t1(S))
    }

    def zero = new Term[Int]{
      def apply[P[_]](implicit S: SystemT[P]) = S.zero
    }

    def succ = new Term[Int => Int]{
      def apply[P[_]](implicit S: SystemT[P]) = S.succ
    }

    def rec[T] = new Term[T => (Int => T => T) => (Int => T)]{
      def apply[P[_]](implicit S: SystemT[P]) = S.rec
    }
  }
}
