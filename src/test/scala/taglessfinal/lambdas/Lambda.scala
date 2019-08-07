package org.hablapps
package gist
package lambdas

trait Lambda[P[_, _]]{
  def vz[E, T]: P[(T, E), T]
  def vs[E, T1, T2](v: P[E, T2]): P[(T1, E), T2]
  def lam[E, T1, T2](body: P[(T1, E), T2]): P[E, T1 => T2]
  def app[E, T1, T2](f: P[E, T1 => T2], a: P[E, T1]): P[E, T2]

  def int[E](i: Int): P[E, Int]
  def add[E](e1: P[E, Int], e2: P[E, Int]): P[E, Int]
}

object Lambda{
  implicit val ShowLambda: Lambda[semantics.Show] = semantics.ShowSem
}
