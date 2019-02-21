package org.hablapps.gist

import scalaz._, Scalaz._

/**
 * State algebras
 */

trait Getter[P[_],S]{
  def apply(): P[S]
}

trait Setter[P[_],S]{
  def apply(t: S): P[Unit]
}

trait Field[P[_],S]{
  val get: Getter[P,S]
  val put: Setter[P,S]

  def modify(f: S => S)(implicit B: Bind[P]): P[Unit] =
    get() >>= { s => put(f(s)) }
}

trait ListP[Alg[_[_],_],P[_],S]{
  val apply: P[List[Alg[P,S]]]

  def traverse[T](
    f: Alg[P,S] => P[T])(implicit
    M: Monad[P]): P[List[T]] =
    apply >>= { _ traverse[P,T] f }

  def traverseZip[A,T](
    values: List[A])(
    f: (Alg[P,S],A) => P[T])(implicit
    M: Monad[P]): P[List[T]] =
    apply >>= { algs =>
      (algs zip values) traverse f.tupled
    }
}

trait StdTraverse[P[_],S] extends ListP[Field,P,S]{
  def getAll(implicit M: Monad[P]): P[List[S]] =
    traverse(_.get())

  def putAll(values: List[S])(implicit M: Monad[P]): P[List[Unit]] =
    traverseZip(values)(_ put _)
}

/**
 * Data Model
 */

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
}

