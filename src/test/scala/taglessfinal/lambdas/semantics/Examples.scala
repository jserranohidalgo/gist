package org.hablapps.gist
package lambdas
package semantics

import org.scalatest._

class Examples[P[_, _]](implicit L: Lambda[P]) extends FlatSpec with Matchers{
  import L._

  val `λx0.(x0)`: P[Unit, Int => Int] =
    lam(vz)

  val `y1`: P[(Int, Unit), Int] =
    vz

  val `λx0.(λx1.((x0+(x1+1)))x0)`: P[Unit, Int => Int] =
    lam(
      app(
        lam(
          add(
            vs(vz): P[(Int, (Int, Unit)), Int],
            add(
              vz: P[(Int, (Int, Unit)), Int],
              int(1)))),
        vz))

  val `λx0.((x0+(x0+1)))`: P[Unit, Int => Int] =
    lam(
      add(
        vz: P[(Int, Unit), Int],
        add(
          vz: P[(Int, Unit), Int],
          int(1))))
}
