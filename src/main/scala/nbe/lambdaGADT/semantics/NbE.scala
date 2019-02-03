package org.hablapps.gist.nbe
package lambdaGADT
package semantics

object FirstVersion{

  sealed abstract class Sem[T]
  case class SynInt(t: Term[Int]) extends Sem[Int]
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

  trait NbE[T]{
    def reify(a: Sem[T]): Term[T]
    def reflect(t: Term[T]): Sem[T]
  }

  object NbE{

    def apply[T](implicit N: NbE[T]) = N

    implicit def lamNbE[A: NbE, B: NbE] = new NbE[A => B]{
      def reify(f: Sem[A => B]): Term[A => B] = f match {
        case LamSem(f) =>
          Lam{ ta: Term[A] =>
            NbE[B].reify(f(NbE[A].reflect(ta)))
          }
      }

      def reflect(f: Term[A => B]): Sem[A => B] =
        LamSem{ a: Sem[A] =>
          NbE[B].reflect(App(f, NbE[A].reify(a)))
        }
    }

    implicit def intNbE = new NbE[Int]{
      def reify(s: Sem[Int]): Term[Int] =
        s match {
          case SynInt(t) => t
        }

      def reflect(t: Term[Int]): Sem[Int] =
        SynInt(t)
    }
  }
}
