package org.hablapps.gist
package university2

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

trait Church[Alg[_[_]], T]{
  type Program[P[_]] = Alg[P] => P[T]

  def apply[P[_]](alg: Alg[P]): P[T]
  def apply[P[_]]: Alg[P] => P[T]
  def apply2[P[_]]: Program[P]
}

trait StoreN[P[_], S]{
  type N
  def get: P[List[A]]
  def put(as: List[A]): P[List[Unit]]
}

trait ListP[Alg[_[_],_],P[_],S]{
  def apply: P[StoreN[P, S]]
  val apply: P[List[Alg[P,S]]]

  def apply[T](f: Alg[P,S] => P[T]): P[List[T]]

  def apply2: λ[t => Church[Alg[?[_],S], t]#Program[P]] ~> λ[t => P[List[t]]]

  def compose[Alg2[_[_], _], T](
    f: Alg[P,S] => Alg2[P, T]): ListP[Alg2, P, T] = ???


  def traverse[T](f: Alg[P,S] => P[T])(implicit
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

import Primitive._

trait Department[P[_],D]{
  val self: Field[P,D]
  val budget: IntegerP[P]
}

trait Department[P[_],D]{
  val self: P[Field[P[List[?]],D]]
  val budget: P[IntegerP[P[List[?]]]]
}

trait University[P[_],U]{
  type D

  val self: Field[P,U]
  val name: StringP[P]
  val deps: ListP[Department,P,D]
}

trait Logic[P[_],U]{
  val univ: University[P,U]

  def doubleBudget(implicit M: Monad[P]): P[List[Unit]] =
    (univ.deps compose (_.budget)).apply2.apply(_.modify(_*2))

  // should be reused
  // def getDepts(implicit M: Monad[P]): P[List[univ.D]] =
  //   univ.deps traverse { _.self.get() }
}
