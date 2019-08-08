package org.hablapps.gist
package lambda
package taglessfinal
package debruijn
package partialeval

import org.scalatest._

class PartialEvalSpec extends FlatSpec with Matchers{

  val Examples = new Examples[PartialEval[ShowB, ?, ?]]()(PartialEval.PartialEvalLambda[ShowB])

  "Partial evaluation" should "work" in {


  }
}
