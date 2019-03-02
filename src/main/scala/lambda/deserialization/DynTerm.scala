package org.hablapps.gist
package lambda
package deserialization

// import taglessfinal.debruijn._

abstract class DynTerm[P[_, _], E]{
  type A
  val typ: TQ[A]
  val term: P[E, A]
}

object DynTerm{

  def apply[P[_, _], _A, E](_typ: TQ[_A], _term: P[E, _A]): DynTerm[P, E] =
    new DynTerm[P, E]{
      type A = _A
      val typ = _typ
      val term = _term
    }
}
