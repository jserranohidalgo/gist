package org.hablapps.gist.nbe
package systemT
package semantics

import org.scalatest._

class NonStdSpec extends FunSpec with Matchers{

  val T = Examples[NonStd]()
  import T._

  describe("Reified terms"){

    it("works"){
      NonStd.reify(I[Int]).apply[Show] shouldBe
        "((SK)K)"

      NonStd.reify(B[Int, Boolean, String])[Show] shouldBe
        "((S(KS))K)"

      NonStd.reify(add)[Show] shouldBe
        "((Srec)(K(Ks)))"

      NonStd.reify(NonStd.Sem.app(add, NonStd.Sem.zero))[Show] shouldBe
        "((rec0)(Ks))"

      NonStd.reify(NonStd.Sem.app(NonStd.Sem.app(add, NonStd.Sem.zero), NonStd.Sem.zero))[Show] shouldBe
        "0"
    }
  }
}
