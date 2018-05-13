package org.hablapps.gist.traversal
package concrete

import shapeless.Nat

trait Traversal[S, A]{

  trait Extract[T <: S]{

    trait Result{
      type Content <: ListN[A]
      def getAll(): Content
      def putAll(values: Content): T
    }
    
    type Out <: Result
    def apply(t: T): Out
  }

  object Extract{
    type Aux[T <: S, _Out <: Extract[T]#Result] = Extract[T]{ type Out = _Out }
  }
  
  def apply[T <: S](t: T)(implicit E: Extract[T]): E.Out = E(t)
}