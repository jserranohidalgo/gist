package org.hablapps.gist.traversal
package concrete

import shapeless.Nat

trait Traversal[S, A]{

  trait Result[In <: S]{
    type Content <: ListN[A]
    def getAll(): Content
    def putAll(values: Content): In
  }

  object Result{
    type Aux[In <: S, _Content <: ListN[A]] = Result[In]{ type Content = _Content }
  }

  trait Extract[In <: S]{
    type Out <: Result[In]
    def apply(t: In): Out
  }

  object Extract{
    type Aux[In <: S, _Out <: Result[In]] = Extract[In]{ type Out = _Out }
  }

  def apply[In <: S](t: In)(implicit E: Extract[In]): E.Out = E(t)
}
