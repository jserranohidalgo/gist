package org.hablapps.gist
package lambda
package deserialization

import org.scalatest._

class TreeSpec extends FunSpec with Matchers{

  tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_app(tr_vr("x0"), tr_int(1))) shouldBe
    Node("Lam", List(Leaf("x0"), Node("TArr", List(Node("TInt", List()), Node("TInt", List()))),
      Node("App", List(
        Node("Var", List(Leaf("x0"))),
        Node("Int", List(Leaf("1")))))))
}