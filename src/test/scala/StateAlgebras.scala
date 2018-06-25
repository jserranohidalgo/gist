package org.hablapps.gist
package university

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

import Primitive._

trait Department[P[_],D]{
  val self: Field[P,D]
  val budget: IntegerP[P]
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
    univ.deps traverse { _.budget.modify(_*2) }
  
  // should be reused
  def getDepts(implicit M: Monad[P]): P[List[univ.D]] = 
    univ.deps traverse { _.self.get() }
}

/* Data instance */


case class SUniversity(name: String, departs: Map[String, SDepartment])

case class SDepartment(budget: Double)

// object StateUniversity extends University[State[University, ?], University]{ 
//   univ =>
  //   val M = MonadState[State[University, ?], University]

  //   def getName = M.gets(_.name)
  //   def setName(name: String) = M.modify(_.copy(name = name))

  //   type Dep = Department

  //   val Deps = M.gets( u => 
  //     new DepartmentsRepo[State[University,?], Dep]{
  //       val result = u.departs.toList map { case (name, dep) =>
  //         new DepartmentRepo[State[University, ?], Department] {
  //           val M: MonadState[State[University, ?], Department] =
  //             new MonadState[State[University, ?], Department]{
  //               def point[A](a: => A) = State.state[University, A](a)
  //               def bind[A,B](fa: State[University,A])(f: A => State[University,B]) =
  //                 Monad[State[University,?]].bind(fa)(f)
  //               def init = get
  //               def get() = State.state[University, Department](dep)
  //               def put(dep: Department) = univ.M.modify{ univ =>
  //                 univ.copy(departs = univ.departs.updated(name, dep))
  //               }
  //             }

  //           def getBudget =
  //             M.gets(_.budget)

  //           def setBudget(budget: Double) =
  //             M.modify(_ => Department(budget))
  //         }
  //       }
  //     })
  // }

