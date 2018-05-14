package org.hablapps.gist.traversal
package concrete

import shapeless.{_0, Succ, Nat}

sealed abstract class ListN[A]
case class Nil[A]() extends ListN[A]
case class ::[A, L <: ListN[A]](head: A, tail: L) extends ListN[A]

object ListN{
  
  trait Length[A, L <: ListN[A]]{
    type Out <: Nat
  }

  object Length{
    type Aux[A, L <: ListN[A], N <: Nat] = Length[A,L]{ type Out = N }

    implicit def nilLength[A] = new Length[A,Nil[A]]{
      type Out = _0
    }

    implicit def consLength[A, T <: ListN[A]](implicit tailL: Length[A,T]) = 
      new Length[A, A :: T]{
        type Out = Succ[tailL.Out]
      }
  }

  trait Concatenate[A, L1 <: ListN[A], L2 <: ListN[A]]{
    type Out <: ListN[A]
    def apply(l1: L1, l2: L2): Out
    def reverse(out: Out): (L1, L2)
  }

  object Concatenate{
    
    type Aux[A, L1 <: ListN[A], L2 <: ListN[A], L <: ListN[A]] = Concatenate[A,L1,L2]{
      type Out = L
    }

    def apply[A, L1 <: ListN[A], L2 <: ListN[A]](
      l1: L1, l2: L2)(implicit
      conc: Concatenate[A, L1, L2]): conc.Out =
      conc(l1,l2)

    implicit def nilCase[A, L2 <: ListN[A]] =
      new Concatenate[A, Nil[A], L2]{
        type Out = L2
        def apply(l1: Nil[A], l2: L2) = l2
        def reverse(out: L2): (Nil[A],L2) = (Nil(),out)
      }

    implicit def consCase[A, L1 <: ListN[A], L2 <: ListN[A]](implicit
      conc: Concatenate[A, L1, L2]) =
      new Concatenate[A, ::[A, L1], L2]{
        type Out = ::[A, conc.Out]
        def apply(l1: ::[A,L1], l2: L2): Out =
          ::(l1.head, conc(l1.tail, l2))
        def reverse(out: ::[A, conc.Out]): (::[A,L1],L2) =
          conc.reverse(out.tail) match {
            case (l1,l2) => (::(out.head, l1),l2)
          }
      }
  }
}