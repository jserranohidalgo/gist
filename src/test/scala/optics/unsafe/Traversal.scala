package org.hablapps.gist.optics
package unsafe

trait Traversal[S,T,A,B]{
  def getAll(s: S): List[A]
  def putAll(s: S): List[B] => T 
}

object Traversal{
  
  def fromConcrete[S,T,A,B](implicit f: concrete.Traversal[S,T,A,B]) =
    new Traversal[S,T,A,B]{
      def getAll(s: S): List[A] = 
        f.extract(s).getAll
      def putAll(s: S): List[B] => T = 
        f.extract(s).putAll
    }

  import scalaz.std.list._, scalaz.Const
    
  // TODO: use FunList as an applicative
  def fromVL[S,T,A,B](vl: vanLaarhoven.Traversal[S,A]) = 
    new Traversal[S,T,A,B]{
      def getAll(s: S): List[A] = 
        vl[Const[List[A],?]](a => Const(List(a))).apply(s).getConst
      def putAll(s: S): List[B] => T = ???
    }
}