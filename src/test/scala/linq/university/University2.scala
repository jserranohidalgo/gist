package org.hablapps.gist

import scalaz._, Scalaz._
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

trait Logic2[P[_],U]{
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

