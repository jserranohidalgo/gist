package org.hablapps.gist.optics
package concrete

trait Traversal[S,T,A,B]{
  def extract(s: S): Traversal.FunList[A,B,T]
}

object Traversal{

  sealed abstract class FunList[A,B,T]
  case class Done[A,B,T](t: T) extends FunList[A,B,T]
  case class More[A,B,T](a: A, f: FunList[A,B,B => T]) extends FunList[A,B,T]

  object FunList{
    implicit class FunListOps[A,B,T](f: FunList[A,B,T]){   
      def getAll(): List[A] = f match {
        case Done(_) => List()
        case More(a,f) => a::f.getAll
      }

      def putAll(l: List[B]): T = (l,f) match {
        case (Nil, Done(t)) => t
        case (head :: tail, More(_, f)) => f.putAll(tail)(head)
        // otherwise, undefined
      }
    }
  }
  
  
  trait Laws[S,T,A,B]{
    val T: Traversal[S,T,A,B]

    // Precondition: b.length == T.extract(s).getAll.length
    def putget(s: S, b: List[B]): Boolean
    def getput(s: S): Boolean 
    def putput(s: S, b1: List[B], b2: List[B]): Boolean
  }
}