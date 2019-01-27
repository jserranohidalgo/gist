package org.hablapps.gist.nbe
package lambda
package semantics

import org.scalatest._

class ShowSpec extends FunSpec with Matchers{

  val L = Examples[Show]()
  import L._

  describe("Lambda expressions"){
    it("evaluate to standard functions over typed environments"){

      ex1[Unit](0) shouldBe "(1+3)"

      ex2[Unit](0) shouldBe "λx0.(x0+x1)"

      ex4(0) shouldBe "(x0+x1)"

      ex3[Unit](0) shouldBe "λx0.((x0 1)+2)"
    }
  }
}
