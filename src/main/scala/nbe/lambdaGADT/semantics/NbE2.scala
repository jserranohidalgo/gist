package org.hablapps.gist.nbe
package lambdaGADT
package semantics

sealed abstract class Sem[T]
case class SynInt(t: Nf[Int]) extends Sem[Int]
case class LamSem[A, B](t: Sem[A] => Sem[B]) extends Sem[A => B]

object Sem{
  object syntax {
    implicit class SemApp[A, B](f: Sem[A => B]){
      def apply(a: Sem[A]): Sem[B] = f match {
        case LamSem(f) => f(a)
      }
    }
  }
}

sealed abstract class Nf[_]
case class LamNf[A, B](f: Ne[A] => Nf[B]) extends Nf[A => B]
case class IntNf(i: Int) extends Nf[Int]
case class AddNf(e1: Nf[Int], e2: Nf[Int]) extends Nf[Int]
case class Coerce[Int](at: Ne[Int]) extends Nf[Int]

sealed abstract class Ne[_]
case class AppNe[A, B](f: Ne[A => B], a: Nf[A]) extends Ne[B]

object Ne{
  object syntax{
    implicit class NfApp[A, B](f: Ne[A => B]){
      def apply(a: Nf[A]): Ne[B] =
        AppNe(f, a)
    }
  }
}

trait NbE[A]{
  def reify(sem: Sem[A]): Nf[A]
  def reflect(ne: Ne[A]): Sem[A]
}

object NbE{

  def apply[T](implicit N: NbE[T]) = N

  import Sem.syntax._
  import Ne.syntax._

  implicit def LamNbE[A: NbE, B: NbE] = new NbE[A => B]{

    def reify(sem: Sem[A => B]): Nf[A => B] =
      LamNf{ ne: Ne[A] =>
        NbE[B].reify(sem(NbE[A].reflect(ne)))
      }

    def reflect(ne: Ne[A => B]): Sem[A => B] =
      LamSem{ semA: Sem[A] =>
        NbE[B].reflect(ne((NbE[A].reify(semA))))
      }
  }

  implicit val IntNbE = new NbE[Int]{
    def reify(sem: Sem[Int]): Nf[Int] =
      sem match {
        case SynInt(t) => t
      }

    def reflect(ne: Ne[Int]): Sem[Int] =
      SynInt(Coerce(ne))
  }
}
