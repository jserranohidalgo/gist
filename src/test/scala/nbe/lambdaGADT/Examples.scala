package org.hablapps.gist.nbe
package lambdaGADT

object Examples{

  val add5: Term[Int => Int] =
    Lam{ x: Term[Int] => Add(x, IntT(5)) }

  type Nat[A] = (A => A) => A => A

  def zero[A]: Term[Nat[A]] =
    Lam{ succ: Term[A => A] =>
      Lam{ zero: Term[A] =>
        zero
      }
    }

  def succ[A]: Term[Nat[A] => Nat[A]] =
    Lam{ nat: Term[Nat[A]] =>
      Lam{ succ: Term[A => A] =>
        Lam{ zero: Term[A] =>
          App(succ, App(App(nat, succ), zero))
        }
      }
    }

  def three[A]: Term[Nat[A]] =
    App(succ[A], App(succ[A], App(succ[A], zero[A])))

  def add[A]: Term[Nat[A] => Nat[A] => Nat[A]] =
    Lam{ n: Term[Nat[A]] =>
      Lam{ m: Term[Nat[A]] =>
        Lam{ s: Term[A => A] =>
          Lam{ z: Term[A] =>
            App(App(m, s), App(App(n, s), z))
          }
        }
      }
    }

}
