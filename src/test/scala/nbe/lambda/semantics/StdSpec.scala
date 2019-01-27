package org.hablapps.gist.nbe
package lambda
package semantics

import org.scalatest._

class StdSpec extends FunSpec with Matchers{

  val L = Examples[Function1]()
  import L._

  describe("Lambda expressions"){
    it("evaluate to standard functions over typed environments"){

      ex1[Unit](()) shouldBe 4

      ex2[Unit]((3, ()))(4) shouldBe 7

      ex3[Unit](())(_ + 1) shouldBe 4
    }
  }
}
