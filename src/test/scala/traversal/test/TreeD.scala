package org.hablapps.gist.traversal
package test

import shapeless.{Nat, Succ, _0}
import shapeless.ops.nat.Sum

sealed abstract class Tree[A]
case class Leaf[A]() extends Tree[A]
case class Node[T1 <: Tree[A], A, T2 <: Tree[A]](
  left: T1, root: A, right: T2) extends Tree[A]

object Tree{

  import concrete.{Nil, ListN, ::}, ListN.Concatenate

  class Of[A,B]{
    implicit object InOrder extends concrete.Traversal[Tree[A],Tree[B],A,B]{

      implicit val leafInOrder = new Extract[Leaf[A]]{
        type Out = Result.Aux[Leaf[B], _0]

        def apply(tree: Leaf[A]) = new Result{
          type Out = Leaf[B]
          type N = _0

          def getAll() = Nil()
          def putAll(nil: ListN.Aux[B,_0]) = Leaf()
        }
      }

      implicit def nodeInOrder[
        LT <: Tree[A],
        LL <: ListN[A],
        RT <: Tree[A],
        RL <: ListN[A]](implicit
        extractLeft: Extract.Aux[LT, Result.Aux[LT,LL]],
        extractRight: Extract.Aux[RT, Result.Aux[RT,RL]],
        concatenate: Concatenate[A, LL, A::RL]) =

        new Extract[Node[LT,A,RT]]{
          type Out = Result.Aux[Node[LT,B,RT], concatenate.N]

          def apply(tree: Node[LT,A,RT]) = new Result{
            type Out = Node[LT,B,RT]
            type N = concatenate.N

            def getAll(): ListN.Aux[A,N] =
              concatenate(extractLeft(tree.left).getAll,
                ::(tree.root, extractRight(tree.right).getAll))

            def putAll(content: ListN.Aux[B,N]) = ???
              // concatenate.reverse(content) match {
              //   case (ll, ::(b, rl)) =>
              //     Node(extractLeft(tree.left).putAll(ll), b, extractRight(tree.right).putAll(rl))
              // }
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

// object Tree{
//   type Aux[A, _N <: Nat] = Tree[A]{ type N = _N }

//   object Syntax{
//     def leaf[A]() = Leaf[A]()
//     def node[T1 <: Tree[A], A, T2 <: Tree[A]](
//       l: T1, a: A, r: T2)(implicit
//       _sum: Sum[l.N,r.N]) = new Node[l.type,A,r.type](l,a,r){
//       val sum = _sum
//     }
//   }

//   class TreeTraversal[A] extends concrete.Traversal[Aux[A,?],A]{

//     // implicit def leafGetAll[A] = new GetAll[_0,Leaf[A]]{
//     //   // type Out = treedep.Nil[A]
//     //   def apply(t: Leaf[A]) = ??? // Nil()
//     // }

//     // implicit def nodeGetAll[A,
//     //   NL <: Nat, L <: Tree.Aux[A,NL], LL <: ListN[A],
//     //   NR <: Nat, R <: Tree.Aux[A,NR], LR <: ListN[A]](implicit
//     //   gl: GetAll.Aux[NL,L,LL],
//     //   gr: GetAll.Aux[NR,R,LR],
//     //   sum: Sum[NL,NR],
//     //   concat: ListN.Concatenate[A,LL,A::LR]) = new GetAll[Succ[sum.Out], Node[L,A,R]]{
//     //     type Out = concat.Out
//     //     def apply(t: Node[L,A,R]) =
//     //       concat(gl(t.left), ::(t.root, gr(t.right)))
//     //   }
//   }

//   import scalaz.Applicative, scalaz.syntax.applicative._

//   class InOrder[A] extends vanLaarhoven.TraversalD[Tree[A], A]{

//     implicit object fromLeaf extends Case[Leaf[A]]{
//       type S2 = Leaf[A]
//       def apply[F[_]: Applicative](f: A => F[A]) =
//         _ => Leaf[A]().point[F]
//     }

//     implicit def fromNode[
//       L1 <: Tree[A],
//       R1 <: Tree[A],
//       NL2 <: Nat,
//       L2 <: Tree.Aux[A,NL2],
//       NR2 <: Nat,
//       R2 <: Tree.Aux[A,NR2]](implicit
//       C1: Case.Aux[L1, L2],
//       C2: Case.Aux[R1, R2],
//       sum2: Sum[NL2, NR2]) = new Case[Node[L1,A,R1]]{
//         type S2 = Node[L2,A,R2]
//         def apply[F[_]: Applicative](f: A => F[A]) = {
//           case n@Node(left, root, right) =>
//             (C1(f).apply(left) |@| f(root) |@| C2(f).apply(right))(
//               (fl,fa,fr) => new Node[L2,A,R2](fl,fa,fr){ val sum = sum2 }
//             )
//         }
//       }
//   }
// }