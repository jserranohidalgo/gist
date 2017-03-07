package org.hablapps.gist

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object monad{
  import cats.Monad

  def apply[P[_]: Monad,T](t: T): P[T] = macro impl[P,T]

  def impl[P[_], T](
    c: whitebox.Context)(
    t: c.Expr[T])(
    M: c.Expr[Monad[P]])(implicit 
    e1: c.WeakTypeTag[P[_]],
    e2: c.WeakTypeTag[T]): c.Expr[P[T]] = {
      import c.universe._
      c.Expr[P[T]](q"$M.pure($t)")
    }
}