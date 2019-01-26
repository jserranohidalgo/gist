package org.hablapps.gist.nbe
package systemT
package semantics

import org.scalatest._

class ShowSpec extends FunSpec with Matchers{

  val T = Examples[λ[T => String]]()

  describe("Show terms"){

    it("works"){
      T.I[Int] shouldBe "((SK)K)"
      T.B[Int, Boolean, String] shouldBe "((S(KS))K)"
    }
  }
}
