package org.hablapps.gist.nbe
package lambdaGADT
package semantics

object Show{

  def apply[T](term: Term[T]): Int => String =
    term match {
      case IntT(i) => _ =>
        i.toString
      case Add(i, j) => c =>
        (Show(i)(c) + "+" + Show(j)(c))
      case Var(x) => _ =>
        x
      case Lam(f) => c => {
        val x = "x" + c
        "(λ" + x + "." + Show(f(Var(x)))(c+1) + ")"
      }
      case App(f, a) => c =>
        "(" + Show(f)(c) + Show(a)(c) + ")"
    }
}
