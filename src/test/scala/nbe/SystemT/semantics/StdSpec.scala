package org.hablapps.gist.nbe
package systemT
package semantics

import org.scalatest._

class StdSpec extends FunSpec with Matchers{

  val T = Examples[cats.Id]()
  import T._

  describe("arithmetic functions"){
    it("work"){

      add(0)(0) shouldBe 0

      add(1)(1) shouldBe 2
    }
  }
}
