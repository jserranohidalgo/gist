package org.hablapps.gist
package lambda
package taglessfinal
package debruijn
package semantics

import org.scalatest._

class StdSpec extends FunSpec with Matchers{

  val L = Examples[Function1]()
  import L._

  describe("Evaluate lambda expressions - tagless final - de Bruijn"){
    it("work over typed environments"){

      ex1[Unit](()) shouldBe 4

      ex2[Unit]((3, ()))(4) shouldBe 7

      ex4((3, (4, ()))) shouldBe 7

      ex3[Unit](())(_ + 1) shouldBe 4
    }
  }
}
