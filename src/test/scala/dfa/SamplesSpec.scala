package dfa

import org.scalatest._

class SamplesSpec extends FunSpec with Matchers{
  import Samples._

  describe("DfAs"){
    it("work"){
      DFA.run(`(01)*`)(List(s0, s1)) shouldBe q0
    }

    it("accepts"){
      DFA.accept(`(01)*`)(List()) shouldBe true
      DFA.accept(`(01)*`)(List(s0)) shouldBe false
      DFA.accept(`(01)*`)(List(s0, s1)) shouldBe true
      DFA.accept(`(01)*`)(List(s0, s1, s0, s1)) shouldBe true
    }
  }
}
