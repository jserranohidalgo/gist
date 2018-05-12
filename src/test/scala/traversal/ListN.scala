package org.hablapps.gist.traversal
package treedep

import shapeless.{_0, Succ, Nat}

sealed abstract class ListN[A, N <: Nat]
case class Nil[A]() extends ListN[A, _0]
case class ::[A, N <: Nat, L <: ListN[A,N]](head: A, tail: L) extends ListN[A, Succ[N]]

object ListN{

  trait Concatenate[
    A, N1 <: Nat, L1 <: ListN[A, N1], 
    N2 <: Nat, L2 <: ListN[A, N2]]{
    type N <: Nat
    type Out <: ListN[A, N]
    def apply(l1: L1, l2: L2): Out
  }

  object Concatenate{
    implicit def nilCase[A, N2 <: Nat, L2 <: ListN[A, N2]] = 
      new Concatenate[A, _0, Nil[A], N2, L2]{
        type N = N2
        type Out = L2
        def apply(l1: Nil[A], l2: L2) = l2
      }

    implicit def consCase[
      A, N1 <: Nat, L1 <: ListN[A, N1], 
      N2 <: Nat, L2 <: ListN[A, N2]](implicit 
      conc: Concatenate[A, N1, L1, N2, L2]) =
      new Concatenate[A, Succ[N1], ::[A, N1, L1], N2, L2]{
        type N = Succ[conc.N]
        type Out = ::[A, conc.N, conc.Out]
        def apply(l1: ::[A,N1,L1], l2: L2): Out = 
          ::(l1.head, conc(l1.tail, l2))
      }
  }
}