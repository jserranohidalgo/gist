package org.hablapps.gist.optics
package typesafe

import shapeless.{_0, Succ, Nat}

sealed abstract class List[A]
case class Nil[A]() extends List[A]
case class ::[A, L <: List[A]](head: A, tail: L) extends List[A]

object List{
  
  trait Length[A, L <: List[A]]{
    type Out <: Nat
  }

  object Length{
    type Aux[A, L <: List[A], N <: Nat] = Length[A,L]{ type Out = N }

    implicit def nilLength[A] = new Length[A,Nil[A]]{
      type Out = _0
    }

    implicit def consLength[A, T <: List[A]](implicit tailL: Length[A,T]) = 
      new Length[A, A :: T]{
        type Out = Succ[tailL.Out]
      }
  }

  trait Concatenate[A, L1 <: List[A], L2 <: List[A]]{
    type Out <: List[A]
    def apply(l1: L1, l2: L2): Out
    def reverse(out: Out): (L1, L2)
  }

  object Concatenate{
    
    type Aux[A, L1 <: List[A], L2 <: List[A], L <: List[A]] = Concatenate[A,L1,L2]{
      type Out = L
    }

    def apply[A, L1 <: List[A], L2 <: List[A]](
      l1: L1, l2: L2)(implicit
      conc: Concatenate[A, L1, L2]): conc.Out =
      conc(l1,l2)

    implicit def nilCase[A, L2 <: List[A]] =
      new Concatenate[A, Nil[A], L2]{
        type Out = L2
        def apply(l1: Nil[A], l2: L2) = l2
        def reverse(out: L2): (Nil[A],L2) = (Nil(),out)
      }

    implicit def consCase[A, L1 <: List[A], L2 <: List[A]](implicit
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