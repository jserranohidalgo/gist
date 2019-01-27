package org.hablapps.gist.nbe
package systemT
package semantics

import org.scalatest._

class TermSpec extends FunSpec with Matchers{

  val T = Examples[Term]()
  import T._

  describe("Church terms"){

    it("works"){
      Term.norm(I[Int])[Show] shouldBe
        "((SK)K)"

      Term.norm(B[Int, Boolean, String])[Show] shouldBe
        "((S(KS))K)"
    }
  }

  describe("normalisation"){
    it("work with Show"){

      Term.norm(Term.Sem.app(
        I[(Boolean => String) => (Int => Boolean) => (Int => String)],
        B[Int, Boolean, String]))[Show] shouldBe
        "((S(KS))K)"
    }

    it("work with evaluation"){

      val f: (Boolean => String) => (Int => Boolean) => (Int => String) =
        Term.norm(Term.Sem.app(
          I[(Boolean => String) => (Int => Boolean) => (Int => String)],
          B[Int, Boolean, String]))[cats.Id]

      val isEven: Int => String =
        f(b => if (b) "true" else "false")(_ % 2 == 0)

      isEven(4) shouldBe "true"
      isEven(1) shouldBe "false"
    }
  }

  describe("arithmetic functions"){
    it("work"){

      Term.norm(Term.Sem.app(Term.Sem.app(add, Term.Sem.zero), Term.Sem.zero))[Show] shouldBe "0"

      val _1 = Term.Sem.app(Term.Sem.succ, Term.Sem.zero)

      Term.norm(Term.Sem.app(Term.Sem.app(add, _1), _1))[Show] shouldBe "(s(s0))"
    }
  }
}
