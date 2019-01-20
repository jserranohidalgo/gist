package org.hablapps.gist.nbe
package systemT

case class Examples[P[_]](implicit ST: SystemT[P]){
  import ST._

  def I[T1]: P[T1 => T1] =
    S[T1, Unit => T1, T1](K[T1, Unit => T1])(K[T1, Unit])
    // S(K)(K)
    // (((S[T1, (?6 => ?5), T1]: P[(T1 => (?6 => ?5) => T1) => (T1 => (?6 => ?5)) => T1 => T1])
    //   .apply(K[T1, (?6 => ?5)]: P[T1 => (?6 => ?5) => T1]) : P[(T1 => (?6 => ?5)) => T1 => T1])
    //   .apply(K[?5, ?6]: P[?5 => ?6 => ?5]) : P[T1 => T1])
    // (((S[T1, (Unit => T1), T1]: P[(T1 => (Unit => T1) => T1) => (T1 => (Unit => T1)) => T1 => T1])
    //   .apply(K[T1, (Unit => T1)]: P[T1 => (Unit => T1) => T1]) : P[(T1 => (Unit => T1)) => T1 => T1])
    //   .apply(K[T1, Unit]: P[T1 => Unit => T1]) : P[T1 => T1])

  def B[T1, T2, T3]: P[(T2 => T3) => (T1 => T2) => (T1 => T3)] =
    ???

  def C[T1, T2, T3](f: P[T1 => T2 => T3])(g: P[T2]): P[T1 => T3] =
    ???
}
