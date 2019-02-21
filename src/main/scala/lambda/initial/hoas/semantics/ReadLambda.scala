package org.hablapps.gist
package lambda
package initial
package hoas
package semantics

import scala.language.existentials

object ReadLambda{

  val lambdaR = """\(λ(.*?)\.(.*)\)""".r
  val appR = """\((.*) (.*)\)""".r
  val intR = """\d+""".r
  val addR = """.*?\+.*?""".r

  def apply(s: String)(env: Map[String, _]): Lambda[Any, Any] = s match {
    case lambdaR(v, b) =>
      Lam[Any, Nothing, Any]{ t1 => apply(b)(env + (v -> t1))}
    case appR(f, a) =>
      App[Any, Nothing, Any](apply(f)(env), apply(a)(env))
    case intR(i) =>
      IntL(Integer.parseInt(i))
    case addR(i, j) =>
      Add(apply(i)(env), apply(j)(env))
    case s =>
      Var(env(s))
  }
}
