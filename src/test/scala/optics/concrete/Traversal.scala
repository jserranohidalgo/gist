package org.hablapps.gist.optics
package concrete

trait Traversal[S,A]{
  def getAll(s: S): List[A]
}

object Traversal{

  object Syntax extends vanLaarhoven.TraversalSyntax{
    implicit class TraversalOps[S](s: S){
      def getAll[A]()(implicit T: Traversal[S,A]): List[A] = 
        T.getAll(s)
    }
  }
}