package org.hablapps.gist

import scala.reflect.macros._
import scala.language.experimental.macros

/*
This gist implements a macro to write monadic programs without using
neither explicit `flatMap`s nor for-comprehensions, but conventional 
`val` definitions, semicolons and `return` expressions. We wrote this 
macro just with a didactic purpose: showing that monadic code is 
simple imperative code. This is so much true that the conventional 
imperative syntax of Scala can be used to write monadic code. 
You can find some examples in the following file.

https://github.com/hablapps/gist/blob/master/src/test/scala/ADTs.scala

Last, note that this macro is far from being complete. We just included
coverage for typical use cases that allows us to illustrate our claim. 
*/
object monad{
  import cats.Monad

  /* 
  This macro allows us to transform a block of conventional imperative code
  into an imperative program over monad `P`
  */
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
          case Block(head::tail,i) =>
            val (name, tpe, value) = head match {
              case q"val $name: $tpe = $value" => (name,tpe,value)
              case q"$value" => (termNames.WILDCARD, tq"Unit", value)
            } 
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