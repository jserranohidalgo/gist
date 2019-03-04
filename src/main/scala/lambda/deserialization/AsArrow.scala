package org.hablapps.gist
package lambda
package deserialization

import scalaz.Leibniz._

abstract class AsArrow[A]{
  type T1
  type T2
  val tA: TQ[A]
  val eq: Option[(TQ[T1], TQ[T2], A === (T1 => T2))]
}

object AsArrow{

  def unapply[A](asA: AsArrow[A]): Option[(TQ[A], Option[(TQ[asA.T1], TQ[asA.T2], A === (asA.T1 => asA.T2))])] =
    Some((asA.tA, asA.eq))

  def apply[A, _T1, _T2](_tA: TQ[A],
      _eq: Option[(TQ[_T1], TQ[_T2], A === (_T1 => _T2))]) = new AsArrow[A]{
    type T1 = _T1
    type T2 = _T2
    val tA = _tA
    val eq = _eq
  }

  implicit def apply[P[_], A](
      asArrow: AsArrow[A]): P[A] => Option[P[asArrow.T1 => asArrow.T2]] =
    pa => asArrow.eq.map(_._3.subst[P](pa))

  implicit val AsArrowTSYM = new TSYM[AsArrow]{
    def tint: AsArrow[Int] =
      AsArrow[Int, Nothing, Nothing](TSYM[TQ].tint, None)

    def tarr[T1, T2](t1: AsArrow[T1], t2: AsArrow[T2]): AsArrow[T1 => T2] =
      AsArrow[T1 => T2, T1, T2](t1.tA -> t2.tA, Some((t1.tA, t2.tA, refl)))
  }
}
