package org.hablapps.gist
package lambdas
package semantics

import org.scalatest._

class ShowSemSpec extends FlatSpec with Matchers{

  val Examples = new Examples[Show]
  import Examples._

  "complex expressions" should "work" in {

    `λx0.(x0)`(0) shouldBe "λx0.(x0)"

    `y1`(0) shouldBe "y1"

    `λx0.(λx1.((x0+(x1+1)))x0)`(0) shouldBe "λx0.(λx1.((x0+(x1+1)))x0)"

    `λx0.((x0+(x0+1)))`(0) shouldBe "λx0.((x0+(x0+1)))"
  }
}
