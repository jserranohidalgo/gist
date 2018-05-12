package org.hablapps.gist.traversal
package treedep

import shapeless.{Nat, Succ, _0}
import shapeless.ops.nat.Sum

sealed abstract class Tree[A]{
  type N <: Nat
}

case class Leaf[A]() extends Tree[A]{
  type N = _0
}

abstract case class Node[T1 <: Tree[A], A, T2 <: Tree[A]](
  left: T1, root: A, right: T2) extends Tree[A]{
  val sum: Sum[left.N, right.N]
  type N = Succ[sum.Out]
}

object Tree{
  type Aux[A, _N <: Nat] = Tree[A]{ type N = _N }

  object Syntax{
    def leaf[A]() = Leaf[A]()
    def node[T1 <: Tree[A], A, T2 <: Tree[A]](
      l: T1, a: A, r: T2)(implicit
      _sum: Sum[l.N,r.N]) = new Node[l.type,A,r.type](l,a,r){
      val sum = _sum
    }
  }


  def InOrder1[A] = new Traversal[Tree[A], A]{
    def getAll(s: Tree[A]): ListN[A, s.N] = ???

    def putAll(s: Tree[A]): ListN[A, s.N] => Tree[A]{ type N = s.N } = ???
  }

  import scalaz.Applicative, scalaz.syntax.applicative._

  class InOrder[A] extends vanLaarhoven.Traversal[Tree[A], A]{

    implicit object fromLeaf extends Case[Leaf[A]]{
      type S2 = Leaf[A]
      def apply[F[_]: Applicative](f: A => F[A]) = 
        _ => Leaf[A]().point[F]
    }

    implicit def fromNode[
      L1 <: Tree[A], 
      R1 <: Tree[A],
      NL2 <: Nat, 
      L2 <: Tree.Aux[A,NL2], 
      NR2 <: Nat, 
      R2 <: Tree.Aux[A,NR2]](implicit
      C1: Case.Aux[L1, L2],
      C2: Case.Aux[R1, R2],
      sum2: Sum[NL2, NR2]) = new Case[Node[L1,A,R1]]{
        type S2 = Node[L2,A,R2]
        def apply[F[_]: Applicative](f: A => F[A]) = {
          case n@Node(left, root, right) => 
            (C1(f).apply(left) |@| f(root) |@| C2(f).apply(right))(
              (fl,fa,fr) => new Node[L2,A,R2](fl,fa,fr){ val sum = sum2 }
            )
        }
      }
  }
}