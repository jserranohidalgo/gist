package org.hablapps.gist.traversal
package concrete

import shapeless.{_0, Succ, Nat}

sealed abstract class ListN[A]{
  type N <: Nat
}
case class Nil[A]() extends ListN[A]{
  type N = _0
}
case class ::[A, L <: ListN[A]](head: A, tail: L) extends ListN[A]{
  type N = Succ[tail.N]
}

object ListN{
  type Aux[A, _N <: Nat] = ListN[A]{ type N = _N }

  trait Concatenate[A, L1 <: ListN[A], L2 <: ListN[A]]{
    type N <: Nat
    type Out <: ListN[A]
    def apply(l1: L1, l2: L2): Out
    def reverse(out: Out): (L1, L2)
  }

  object Concatenate{

    def apply[A, L1 <: ListN[A], L2 <: ListN[A]](
      l1: L1, l2: L2)(implicit
      conc: Concatenate[A, L1, L2]): conc.Out =
      conc(l1,l2)

    implicit def nilCase[A, L2 <: ListN[A]] =
      new Concatenate[A, Nil[A], L2]{
        type N = _0
        type Out = L2
        def apply(l1: Nil[A], l2: L2) = l2
        def reverse(out: L2): (Nil[A],L2) = (Nil(),out)
      }

    implicit def consCase[A, L1 <: ListN[A], L2 <: ListN[A]](implicit
      conc: Concatenate[A, L1, L2]) =
      new Concatenate[A, ::[A, L1], L2]{
        type N = Succ[conc.N]
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