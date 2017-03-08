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

      def lift(b: Block): Tree = 
        b match {
          case Block(List(),i) => 
            q"$M.pure($i)"
          case Block(q"val $name: $tpe = $value"::tail,i) => 
            val r = lift(Block(tail,i))
            q"$M.flatMap($M.pure($value)){ $name: $tpe => $r }"
        }

      val r: Tree = lift(t.tree match {
        case b: Block => b
        case e => Block(List(),e)
      })

      c.Expr[P[T]](r)
    }
}