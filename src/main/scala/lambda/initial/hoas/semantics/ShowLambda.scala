package org.hablapps.gist
package lambda
package initial
package hoas
package semantics

object ShowLambda{

  def apply[T](l: Lambda[ShowH, T]): ShowH[T] =
    FinallyLambda[ShowH].apply(l)
}
