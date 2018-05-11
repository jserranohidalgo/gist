package org.hablapps.gist
package traversal
package vanLaarhoven

import scalaz.Applicative

trait Traversal[S,A]{
  def apply[F[_]: Applicative](f: A => F[A]): S => F[S]
}

trait TraversalSyntax{
  import scalaz.std.list._, scalaz.Const
    
  implicit def toAlg[S,A](t: Traversal[S,A]): traversal.Traversal[S,A] = 
    new traversal.Traversal[S,A]{
      def getAll(s: S): List[A] = 
        t[Const[List[A],?]](a => Const(List(a))).apply(s).getConst
    }
}