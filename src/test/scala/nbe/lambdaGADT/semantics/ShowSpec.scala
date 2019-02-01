package org.hablapps.gist.nbe
package lambdaGADT
package semantics

import org.scalatest._

class ShowSpec extends FunSpec with Matchers{

  import Examples._

  describe("show interpreter"){
    it("work"){
      Show(add5)(0) shouldBe "(λx0.x0+5)"

      Show(zero[Unit])(0) shouldBe "(λx0.(λx1.x1))"
      Show(succ[Unit])(0) shouldBe "(λx0.(λx1.(λx2.(x1((x0x1)x2)))))"
      Show(three[Unit])(0) shouldBe
        "("+Show(succ[Unit])(0)+
          "("+Show(succ[Unit])(0)+
            "("+Show(succ[Unit])(0)+
              Show(zero[Unit])(0)+
            ")"+
          ")"+
        ")"
    }
  }
}
