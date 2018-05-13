package org.hablapps.gist.traversal
package vanLaarhoven

import shapeless._
import scalaz.Applicative

class TraversalD[S, A]{

  trait Case[S1 <: S]{
    type S2 <: S
    def apply[F[_]: Applicative](f: A => F[A]): S1 => F[S2]
  }

  object Case{
    type Aux[S1 <: S, _S2 <: S] = Case[S1]{ type S2 = _S2 }
  }
}

