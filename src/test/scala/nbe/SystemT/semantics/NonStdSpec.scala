package org.hablapps.gist.nbe
package systemT
package semantics

import org.scalatest._

class NonStdSpec extends FunSpec with Matchers{

  val T = Examples[NonStdSem]()

  describe("Reified terms"){

    it("works"){
      NonStdSem.reify(T.I[Int]).apply[λ[T => String]] shouldBe
        "((SK)K)"

      NonStdSem.reify(T.B[Int, Boolean, String])[λ[T => String]] shouldBe
        "((S(KS))K)"
    }
  }
}
