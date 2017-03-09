package org.hablapps.gist

import scala.reflect.macros._
import scala.language.experimental.macros

object monad{
  import cats.Monad

  def apply[P[_]: Monad,T](t: T): P[T] = macro impl[P,T]

  implicit def c[P[_],A](p: P[A]): A = p.asInstanceOf[A]

  implicit class RunOp[P[_],A](program: P[A]){
    def run: A = ??? // not intended to be executed ever, but inside monad macro blocks
  }

  def impl[P[_], T](
    c: whitebox.Context)(
    t: c.Expr[T])(
    M: c.Expr[Monad[P]])(implicit
    e1: c.WeakTypeTag[P[_]],
    e2: c.WeakTypeTag[T]): c.Expr[P[T]] = {
      import c.universe._

      def liftValue(b: Tree): Tree = {
        b match {
          case Select(Apply(_,List(v)),TermName("run")) => v
          case _ => q"$M.pure($b)"
        }
      }

      def liftBlock(b: Block): Tree =
        b match {
          case Block(List(),i) =>
            liftValue(i)
          case Block(q"val $name: $tpe = $value"::tail,i) =>
            val liftedValue = liftValue(value)
            val liftedTail = liftBlock(Block(tail,i))
            q"$M.flatMap($liftedValue){ $name: $tpe => $liftedTail }"
        }

      val untypeT = c.untypecheck(t.tree)

      val r: Tree = liftBlock(untypeT match {
        case b: Block => b
        case e => Block(List(),e)
      })

      c.Expr[P[T]](r)
    }
}