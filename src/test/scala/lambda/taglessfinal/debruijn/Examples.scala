package org.hablapps.gist
package lambda
package taglessfinal
package debruijn

case class Examples[P[_, _]](implicit L: Lambda[P]){

  val `λx0.(x0)`: P[Unit, Int => Int] =
    L.lam(L.vz)

  val `y1`: P[(Int, Unit), Int] =
    L.vz

  val `λx0.(λx1.((x0+(x1+1)))x0)`: P[Unit, Int => Int] =
    L.lam(
      L.app(
        L.lam(
          L.add(
            L.vs(L.vz): P[(Int, (Int, Unit)), Int],
            L.add(
              L.vz: P[(Int, (Int, Unit)), Int],
              L.int(1)))),
        L.vz))

  val `λx0.((x0+(x0+1)))`: P[Unit, Int => Int] =
    L.lam(
      L.add(
        L.vz: P[(Int, Unit), Int],
        L.add(
          L.vz: P[(Int, Unit), Int],
          L.int(1))))

  // tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_app(tr_vr("x0"), tr_int(1)))
  def ex: P[Unit, (Int => Int) => Int] = L.lam(L.app(L.vz[Unit, Int => Int], L.int(1)))

  def ex1: P[(Int => Int, (Int, Unit)), Int] =
    L.app(L.vz[(Int, Unit), Int => Int], L.vs(L.vz))

  // import L._

  // def ex0: P[Unit, Int] =
  //   add(int(1), int(3))

  // def ex1[E]: P[E, Int] =
  //   add(int(1), int(3))

  // def ex2[E]: P[(Int, E), Int => Int] =
  //   lam(add(vz[(Int, E), Int], vs(vz)))

  // def ex4[E]: P[(Int, (Int, E)), Int] =
  //   add(vz[(Int, E), Int], vs(vz))

  // // td3 = lam (add (app z (int 1)) (int 2))
  // def ex3[E]: P[E, (Int => Int) => Int] =
  //   lam(add(app(vz[E, Int => Int])(int(1)), int(2)))
}
