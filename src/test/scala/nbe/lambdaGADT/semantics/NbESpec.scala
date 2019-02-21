package org.hablapps.gist.nbe
package lambdaGADT
package semantics

import org.scalatest._

class NbESpec extends FunSpec with Matchers{
  import FirstVersion._
  import Sem.syntax._

  type Nat[A] = (A => A) => A => A

  def zeroS[A]: Sem[Nat[A]] =
    LamSem{ s: Sem[A => A] =>
      LamSem{ z: Sem[A] =>
        z
      }
    }

  def succS[A]: Sem[Nat[A] => Nat[A]] =
    LamSem{ n: Sem[Nat[A]] =>
      LamSem{ s: Sem[A => A] =>
        LamSem{ z: Sem[A] =>
          s(n(s)(z))
        }
      }
    }

  def threeS[A]: Sem[Nat[A]] =
    succS(succS(succS(zeroS)))

  def addS[A]: Sem[Nat[A] => Nat[A] => Nat[A]] =
    LamSem{ n: Sem[Nat[A]] =>
      LamSem{ m: Sem[Nat[A]] =>
        LamSem{ s: Sem[A => A] =>
          LamSem{ z: Sem[A] =>
            m(s)(n(s)(z))
          }
        }
      }
    }

  describe("Reifying"){
    it("work"){

      Show(NbE[Nat[Int]].reify(threeS[Int]))(0) shouldBe
        "(λx0.(λx1.(x0(x0(x0x1)))))"

      Show(NbE[Nat[Int] => Nat[Int]].reify(addS(zeroS)))(0) shouldBe
        "(λx0.(λx1.(λx2.((x0(λx3.(x1x3)))x2))))"

      // Show(NbE[Nat[Int => Int]].reify(threeS[Int => Int]))(0) shouldBe
      //   "(λx0.(λx1.(x0(x0(x0x1)))))"
    }
  }

  describe("Reflect"){
    import Examples._

    it("work"){

      def norm[T: NbE](t: Term[T]): Term[T] =
        NbE[T].reify(NbE[T].reflect(t))

      Show(norm(three[Int]))(0) shouldBe ""
    }
  }
}
