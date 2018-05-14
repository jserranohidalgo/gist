package org.hablapps.gist.traversal
package test

import shapeless.{Nat, Succ, _0}
import shapeless.ops.nat.Sum

sealed abstract class Tree[A]
case class Leaf[A]() extends Tree[A]
case class Node[T1 <: Tree[A], A, T2 <: Tree[A]](
  left: T1, root: A, right: T2) extends Tree[A]

object Tree{

  import concrete.{Nil, ListN, ::}, ListN.{Length, Concatenate}

  class Of[A,B]{
    implicit object InOrder extends concrete.Traversal[Tree[A],Tree[B],A,B]{

      implicit val leafInOrder = new Extract[Leaf[A]]{
        type Out = Result.Aux[Nil[A],Nil[B],Leaf[B]]

        def apply(tree: Leaf[A]) = new Result{
          type OutGet = Nil[A]
          type InPut = Nil[B]
          type OutPut = Leaf[B]

          type N = _0
          val GetLength: Length.Aux[A,Nil[A],N] = Length.nilLength[A]
          val PutLength: Length.Aux[B,Nil[B],N] = Length.nilLength[B]
    
          def getAll() = Nil()
          def putAll(nil: Nil[B]) = Leaf()
        }
      }

      implicit def nodeInOrder[
        L <: Tree[A],
        LG <: ListN[A],
        LPI <: ListN[B],
        LPO <: Tree[B],
        R <: Tree[A],
        RG <: ListN[A],
        RPI <: ListN[B],
        RPO <: Tree[B],
        CG <: ListN[A],
        CP <: ListN[B],
        _N <: Nat](implicit
        extractL: Extract.Aux[L, Result.Aux[LG,LPI,LPO]],
        extractR: Extract.Aux[R, Result.Aux[RG,RPI,RPO]],
        concG: Concatenate.Aux[A, LG, A::RG, CG],
        concP: Concatenate.Aux[B, LPI, B::RPI, CP],
        lengthG: Length.Aux[A,CG,_N],
        lengthP: Length.Aux[B,CP,_N]) =

        new Extract[Node[L,A,R]]{
          type Out = Result.Aux[CG, CP, Node[LPO,B,RPO]]

          def apply(tree: Node[L,A,R]) = new Result{
            type OutGet = CG
            type InPut = CP
            type OutPut = Node[LPO,B,RPO]
            
            type N = _N
            val GetLength = lengthG
            val PutLength = lengthP
      
            def getAll(): CG =
              concG(extractL(tree.left).getAll,
                ::(tree.root, extractR(tree.right).getAll))

            def putAll(content: CP) =
              concP.reverse(content) match {
                case (ll, ::(b, rl)) =>
                  Node(extractL(tree.left).putAll(ll), b, extractR(tree.right).putAll(rl))
              }
          }
        }
    }
  }

  object Monomorphic{
    def apply[A]: Of[A,A] = new Of[A,A]
  }

  object Polymorphic{
    def apply[A,B]: Of[A,B] = new Of[A,B]
  }
}

