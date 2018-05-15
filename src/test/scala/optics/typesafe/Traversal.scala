package org.hablapps.gist.optics
package typesafe

import shapeless.Nat
import List.Length

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
    type N <: Nat
    type OutGet <: List[A]
    type InPut <: List[B]
    type OutPut <: T
    
    val GetLength: Length.Aux[A,OutGet,N]
    val PutLength: Length.Aux[B,InPut,N]

    def getAll(): OutGet
    def putAll(values: InPut): OutPut
  }

  object Result{
    type Aux[_OutGet <: List[A], _InPut <: List[B], _OutPut <: T] = Result{
      type OutGet = _OutGet
      type InPut = _InPut
      type OutPut = _OutPut
    }
  }

  def apply[In <: S](t: In)(implicit E: Extract[In]): E.Out = E(t)
}
