package org.hablapps.gist.optics
package concrete

trait Affine[S,T,A,B]{
  def extract(s: S): Either[T, (A, B => T)]
}

object Affine{

  trait Laws[S,T,A,B]{
    val A: Affine[S,T,A,B]

    // Precondition: b.isDefined == A.extract(s).isRight
    def putget(s: S, b: Option[B]): Boolean
    def getput(s: S): Boolean 
    def putput(s: S, b1: B, b2: B): Boolean
  }
}