package org.hablapps.gist.traversal
package concrete

import shapeless.Nat

trait Traversal[S1[_ <: Nat], A]{

  trait GetAll[N <: Nat, T <: S1[N]]{
    type Out <: ListN.Aux[A,N]
    def apply(t: T): Out
  }

  object GetAll{
    type Aux[N <: Nat, T <: S1[N], _Out <: ListN.Aux[A,N]] = 
      GetAll[N, T]{ type Out = _Out }
  }
}

object Traversal{

  object Syntax{
    implicit class TraversalOps[N <: Nat, S[_ <: Nat], T <: S[N] ,A](
      t: T)(implicit Trav: Traversal[S,A]){
      def getAll[A]()(implicit GetAll: Trav.GetAll[N,T]) = 
        GetAll(t)
    }
  }
}