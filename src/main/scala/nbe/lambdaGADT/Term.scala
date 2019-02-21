package org.hablapps.gist.nbe
package lambdaGADT

sealed abstract class Term[_]
case class IntT(i: Int) extends Term[Int]
case class Add(e1: Term[Int], e2: Term[Int]) extends Term[Int]
case class Var[T](s: String) extends Term[T]
case class Lam[A, B](f: Term[A] => Term[B]) extends Term[A => B]
case class App[A, B](f: Term[A => B], a: Term[A]) extends Term[B]
