package org.hablapps.gist.nbe
package lambda

case class Examples[P[_, _]](implicit L: Lambda[P]){

  import L._

  def ex1[E]: P[E, Int] =
    add(int(1))(int(3))

  def ex2[E]: P[(Int, E), Int => Int] =
    lam(add(vz[(Int, E), Int])(vs(vz)))

  // td3 = lam (add (app z (int 1)) (int 2))
  def ex3[E]: P[E, (Int => Int) => Int] =
    lam(add(app(vz[E, Int => Int])(int(1)))(int(2)))
}
