package org.hablapps.gist.traversal
package concrete

import shapeless.Nat

trait Traversal[S, T, A, B]{

  trait Extract[In <: S]{
    type Out <: Result
    def apply(t: In): Out
  }

  object Extract{
    type Aux[In <: S, _Out <: Result] =
      Extract[In]{ type Out = _Out }
  }

  trait Result{
    type OutGet <: ListN[A]
    type InPut <: ListN[B]
    type OutPut <: T
    def getAll(): OutGet
    def putAll(values: InPut): OutPut
  }

  object Result{
    type Aux[_OutGet <: ListN[A], _InPut <: ListN[B], _OutPut <: T] = Result{
      type OutGet = _OutGet
      type InPut = _InPut
      type OutPut = _OutPut
    }
  }

  def apply[In <: S](t: In)(implicit E: Extract[In]): E.Out = E(t)
}
