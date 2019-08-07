package org.hablapps.gist
package lambda
package taglessfinal
package debruijn
package partialeval

// Ref. http://okmij.org/ftp/tagless-final/course/PE-dB.html

import cats.Id, cats.~>
import PartialEval.EnvT

sealed abstract class PartialEval[P[_, _], E, T]

case class Unk[P[_, _], E, T](dyn: P[E, T])
  extends PartialEval[P, E, T]

case class Static[P[_, _], E, T](t: T, r: Id ~> P[?, T])
  extends PartialEval[P, E, T]

case class StaFun[P[_, _], Ein, T1, T2](f: (Id ~> λ[Eout => EnvT[P, (T1, Ein), Eout] => PartialEval[P, Eout, T2]]))
  extends PartialEval[P, Ein, T1 => T2]

case class Open[P[_, _], Ein, T](f: (Id ~> λ[Eout => EnvT[P, Ein, Eout] => PartialEval[P, Eout, T]]))
  extends PartialEval[P, Ein, T]


object PartialEval{

  sealed abstract class EnvT[P[_, _], Ein, EOut]
}
