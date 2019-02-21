package org.hablapps.gist
package lambda
package taglessfinal
package hoas
package semantics

import org.scalatest._

class StdSpec extends FunSpec with Matchers{

  val L = Examples[cats.Id]()
  import L._

  describe("Evaluate lambda expressions - tagless final - HOAS"){

    it("works"){

      ex1 shouldBe 4

      // ex2[Unit]((3, ()))(4) shouldBe 7

      // ex4((3, (4, ()))) shouldBe 7

      ex3(_ + 1) shouldBe 4
    }
  }
}
