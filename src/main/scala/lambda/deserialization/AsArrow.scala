package org.hablapps.gist
package lambda
package deserialization

import scalaz.Leibniz._

abstract class AsArrow[A]{
  type T1
  type T2
  val eq: Option[A === (T1 => T2)]
}

object AsArrow{

  def apply[A, _T1, _T2](_eq: Option[A === (_T1 => _T2)]) = new AsArrow[A]{
    type T1 = _T1
    type T2 = _T2
    val eq = _eq
  }

  implicit def apply[P[_], A](
      asArrow: AsArrow[A]): P[A] => Option[P[asArrow.T1 => asArrow.T2]] =
    pa => asArrow.eq.map(_.subst[P](pa))

  implicit val AsArrowTSYM = new TSYM[AsArrow]{
    def tint: AsArrow[Int] =
      AsArrow[Int, Nothing, Nothing](None)

    def tarr[T1, T2](t1: AsArrow[T1], t2: AsArrow[T2]): AsArrow[T1 => T2] =
      AsArrow[T1 => T2, T1, T2](Some(refl))
  }
}
