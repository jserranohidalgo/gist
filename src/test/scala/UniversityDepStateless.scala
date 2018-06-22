package org.hablapps.gist
package university

import scalaz._, Scalaz._

object UniversityManyDepNat extends App {
  import UniversityManyDep._

  trait StateAlg[P[_],T]{
    def get(): P[T]
    def put(t: T): P[Unit]

    def gets[S](f: T => S)(implicit F: Functor[P]): P[S] = 
      get() map f

    def modify(f: T => T)(implicit M: Bind[P]): P[Unit] = 
      get >>= { t => put(f(t)) }
  }

  object StateAlg{

    def apply[P[_],T](implicit sa: StateAlg[P,T]) = sa

    implicit def fromMS[P[_],T](implicit st: MonadState[P,T]) =
      new StateAlg[P,T]{
        def get() = st.get
        def put(t: T) = st.put(t)
      }

    def fromNat[P[_],Q[_]](nat: P ~> Q) = new(StateAlg[P,?] ~> StateAlg[Q,?]){ 
      def apply[T](sap: StateAlg[P,T]) = new StateAlg[Q,T]{
        def get() = nat(sap.get())
        def put(t: T) = nat(sap.put(t))
      }
    }
  }

  trait DepartmentRepo[P[_], Dep]{
    val M: StateAlg[P, Dep]

    def getBudget: P[Double]
    def setBudget(budget: Double): P[Unit]
  }

  object DepartmentRepo{
  
    def fromNat[P[_],Q[_]](
      nat: P ~> Q) = new (DepartmentRepo[P, ?] ~> DepartmentRepo[Q, ?]){
        def apply[T](dp: DepartmentRepo[P,T]) = new DepartmentRepo[Q,T]{
          val M = StateAlg.fromNat(nat)(dp.M)
          def getBudget = nat(dp.getBudget)
          def setBudget(budget: Double) = nat(dp.setBudget(budget))
        }
      }
  }

  // The most simple instance of DepartmentRepo for Dep = case class Department
  val StateDepDepartmentRepo =
    new DepartmentRepo[State[Department, ?], Department] {
      val M = StateAlg[State[Department, ?], Department]

      def getBudget =
        M.gets(_.budget)

      def setBudget(budget: Double) =
        M.modify(_ => Department(budget))
    }

  // Stateless nat trans for traversals

  def liftStateDep = 
    new (State[Department,?] ~> λ[t=>State[University,List[t]]]){
      def apply[T](sd: State[Department,T]) =
        State.gets[University,List[(String,Department)]](_.departs.toList) >>=
        (_.foldLeft(State.state[University,List[T]](List[T]())){
          case (program, (name, dep)) =>   
            val (newDep, out) = sd(dep)
            for {
              outs <- program
              _ <- State.modify{ univ: University => 
                univ.copy(departs = univ.departs.updated(name, newDep))
              }
            } yield out :: outs
        })
    }


  // State univ department repo

  val stateUnivDepRepo: DepartmentRepo[λ[t=>State[University,List[t]]], Department] = 
    DepartmentRepo.fromNat[State[Department,?],λ[t=>State[University,List[t]]]](
      liftStateDep)(StateDepDepartmentRepo)

  // val univ = new UniversityRepo[State[University, ?], University] { univ =>
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

  // val origin = University(
  //   "Rey Juan Carlos",
  //   Map("math" -> Department(10), "physics" -> Department(9)))
  // println(s"Duplicating budget for: $origin")
  // val result = doubleBudget(univ).exec(origin)
  // println(s"...results in: $result")
}

