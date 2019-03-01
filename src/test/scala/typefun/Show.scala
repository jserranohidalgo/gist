package org.hablapps.gist
package typefun

trait Show[T]{
  def write(t: T): String
}

object Show{

  def apply[A](implicit S: Show[A]): Show[A] = S

  implicit val IntShow = new Show[Int]{
    def write(t: Int): String = t.toString
  }
}
