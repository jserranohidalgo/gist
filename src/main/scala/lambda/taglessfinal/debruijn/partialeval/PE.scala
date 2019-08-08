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

case class Static[P[_, _], E, T](t: T, r: λ[T => Unit] ~> P[?, T])
  extends PartialEval[P, E, T]

case class StaFun[P[_, _], Ein, T1, T2](f: EnvT[P, (T1, Ein), ?] ~> PartialEval[P, ?, T2])
  extends PartialEval[P, Ein, T1 => T2]

case class Open[P[_, _], Ein, T](f: EnvT[P, Ein, ?] ~> PartialEval[P, ?, T])
  extends PartialEval[P, Ein, T]


object PartialEval{

  sealed abstract class EnvT[P[_, _], Ein, EOut]
  case class Dyn[P[_, _], E]() extends EnvT[P, E, E]
  case class Arg[P[_, _], E, T](a: PartialEval[P, E, T]) extends EnvT[P, (T, E), E]
  case class Weak[P[_, _], E, T]() extends EnvT[P, E, (T, E)]
  case class Next[P[_, _], Ein, Eout, T](h: EnvT[P, Ein, Eout]) extends EnvT[P, (T, Ein), (T, Eout)]

  def app_open[P[_, _], Ein, Eout, T](t: PartialEval[P, Ein, T], h: EnvT[P, Ein, Eout]): PartialEval[P, Eout, T] =
    ???

  implicit def PartialEvalLambda[P[_, _]](implicit L: Lambda[P]) =
    new debruijn.Lambda[PartialEval[P, ?, ?]]{

      def int[E](i: Int): PartialEval[P, E, Int] =
        Static(i, λ[λ[T => Unit] ~> P[?, Int]](_ => L.int(i)))

      def add[E](i1: PartialEval[P, E, Int], i2: PartialEval[P, E, Int]): PartialEval[P, E, Int] =
        (i1, i2) match {
          case (Static(te1, td1), Static(te2, td2)) =>
            int(te1 + te2)
          case (Static(0, _), x) =>
            x
          case (x, Static(0, _)) =>
            x
          case (Unk(x), Unk(y)) =>
            Unk(L.add(x, y))
          case (x, y) =>
            Open(λ[EnvT[P, E, ?] ~> PartialEval[P, ?, Int]]{
              h => add(app_open(x, h), app_open(y, h))
            })
        }

      def vz[E, T]: PartialEval[P, (T, E), T] =
        Open(λ[EnvT[P, (T, E), ?] ~> PartialEval[P, ?, T]]{
          case Dyn() => ???
          case Arg(a) => ???
          case Next(h) => ???
          case weak => vz_weak(weak)
        })

      def vz_weak[P[_, _], E, T1, T2](w: Weak[P, (T1, E), T2]): PartialEval[P, (T2, (T1, E)), T1] =
        ???

      def vs[E, T, T1](a: PartialEval[P, E, T]): PartialEval[P, (T1, E), T] =
        ???

      def lam[E, T1, T2](t: PartialEval[P, (T1, E), T2]): PartialEval[P, E, T1 => T2] =
        ???

      def app[E, T1, T2](f: PartialEval[P, E, T1 => T2], t1: PartialEval[P, E, T1]): PartialEval[P, E, T2] =
        ???

    }
}
