package org.hablapps.gist.optics
package unsafe_mono

trait Affine[S,A]{
  def getOption(s: S): Option[A]
  def put(s: S): A => S
}

object Affine{
  
  def fromConcrete[S,A](implicit affine: concrete.Affine[S,S,A,A]) = 
    new Affine[S,A]{
      def getOption(s: S): Option[A] = 
        affine.extract(s).fold(
          _ => Option.empty[A], 
          { case (a, _) => Some(a) }
        )

      def put(s: S): A => S = a => 
        affine.extract(s).fold(
          identity,
          { case (_,f) => f(a) }
        )
    }
}