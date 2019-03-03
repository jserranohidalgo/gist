package org.hablapps.gist
package lambda
package deserialization

import org.scalatest._

class TypecheckSpec extends FunSpec with Matchers with Inside{

  it("Int literals"){
    inside(Typecheck[ShowB, Unit, Unit](tr_int(1), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "1"
    }
  }

  it("Add expressions"){
    inside(Typecheck[ShowB, Unit, Unit](tr_add(tr_int(1), tr_int(2)), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "(1+2)"
    }
  }
}
