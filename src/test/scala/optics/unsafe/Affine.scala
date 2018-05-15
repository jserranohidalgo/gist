package org.hablapps.gist.optics
package unsafe

trait Affine[S,T,A,B]{
  def getOption(s: S): Option[A]
  def putOption(s: S): Option[B] => T 
}

object Affine{
  
  def fromConcrete[S,T,A,B](implicit affine: concrete.Affine[S,T,A,B]) =
    new Affine[S,T,A,B]{
      def getOption(s: S): Option[A] = 
        affine.extract(s).fold(
          _ => Option.empty[A], 
          { case (a, _) => Some(a) }
        )

      def putOption(s: S): Option[B] => T = (_, affine.extract(s)) match {
        case (None, Left(t)) => t
        case (Some(b), Right((_,f))) => f(b)
        // otherwise, undefined
      }
    }
}