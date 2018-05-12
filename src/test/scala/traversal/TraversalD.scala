package org.hablapps.gist.traversal
package treedep

import shapeless.Nat

trait Traversal[S <: { type N <: Nat }, A]{
  def getAll(s: S): ListN[A, s.N]
  def putAll(s: S): ListN[A, s.N] => S{ type N = s.N }
}

object Traversal{

  // object Syntax extends vanLaarhoven.TraversalSyntax{
  //   implicit class TraversalOps[S](s: S){
  //     def getAll[A]()(implicit T: Traversal[S,A]): List[A] = 
  //       T.getAll(s)
  //   }
  // }
}