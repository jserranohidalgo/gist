package org.hablapps.gist
package university

import scalaz._, Scalaz._


object UniversityMany extends App {

  trait UniversityRepo[P[_],Univ] {
    val M: MonadState[P, Univ]

    def getName: P[String]
    def setName(name: String): P[Unit]

    type Dep
    val Deps: P[List[DepartmentRepo[P,Dep]]]

    def getDeps(implicit M: Monad[P]): P[List[Dep]] =
      Deps >>= { _ traverse (_.M.get) }

    def putDeps(deps: List[Dep])(implicit M: Monad[P]): P[List[Unit]] =
      Deps >>= { depsList =>
        (depsList zip deps) traverse {
          case (depD, dep) => depD.M.put(dep)
        }
      }
  }

  trait DepartmentRepo[P[_], Dep]{
    val M: MonadState[P, Dep]

    def getBudget: P[Double]
    def setBudget(budget: Double): P[Unit]
  }

  def doubleBudget[P[_]: Monad, Univ](Univ: UniversityRepo[P,Univ]): P[Unit] =
    Univ.Deps >>= { _.traverse_{ Dep =>
      Dep.getBudget >>= (b => Dep.setBudget(b * 2))
    }}

  case class University(name: String, departs: Map[String, Department])

  case class Department(budget: Double)

  val univ = new UniversityRepo[State[University, ?], University] { univ =>
    val M = MonadState[State[University, ?], University]

    def getName = M.gets(_.name)
    def setName(name: String) = M.modify(_.copy(name = name))

    type Dep = Department

    val Deps = M.gets(_.departs.toList map { case (name, dep) =>
      new DepartmentRepo[State[University, ?], Department] {
        val M: MonadState[State[University, ?], Department] =
          new MonadState[State[University, ?], Department]{
            def point[A](a: => A) = State.state[University, A](a)
            def bind[A,B](fa: State[University,A])(f: A => State[University,B]) =
              Monad[State[University,?]].bind(fa)(f)
            def init = get
            def get() = State.state[University, Department](dep)
            def put(dep: Department) = univ.M.modify{ univ =>
              univ.copy(departs = univ.departs.updated(name, dep))
            }
          }

        def getBudget =
          M.gets(_.budget)

        def setBudget(budget: Double) =
          M.modify(_ => Department(budget))
      }
    })
  }

  // val origin = University(
  //   "Rey Juan Carlos",
  //   Map("math" -> Department(10), "physics" -> Department(9)))
  // println(s"Duplicating budget for: $origin")
  // val result = doubleBudget(univ).exec(origin)
  // println(s"...results in: $result")
}

