package org.hablapps.gist

import scalaz._, Scalaz._
import org.scalatest._

class StateLens{
  
  // // Monad state laws

  // trait MonadStateLaws[P[_],A]{
  //   val M: MonadState[P,A]

  //   def GetGet: P[Boolean] = 
  //     for { a1 <- M.get; a2 <- M.get } yield (a1,a2)
  //     for { a1 <- M.get } yield (a1,a2)
  // }

  // Theory

  trait InitialAlg[Initial,Alg[_]]{
    def alg[X]: Alg[Initial]
    def fold[T](implicit alg: Alg[T]): Initial => T
  }

  trait FreeAlg[Free[_],Alg[_]]{
    def alg[X]: Alg[Free[X]]
    def unit[X](x: X): Free[X]
    def foldMap[X,T](f: X =>T)(implicit alg: Alg[T]): Free[X] => T
  }

  object FreeAlg{
    implicit def FreeAlg[F[_],Alg[_],X](implicit Free: FreeAlg[F,Alg]): Alg[F[X]] = Free.alg[X]
  }

  // Practice

  object IntMonoidExpr{
    def apply(i1: Int, i2: Int): Int = i1 |+| i2 |+| mzero[Int]
  }

  object StringMonoidExpr{
    def apply(i1: String, i2: String): String = i1 |+| i2 |+| mzero[String]
  }

  object TMonoidExpr{
    def apply[T: Monoid](i1: T, i2: T): T = i1 |+| i2 |+| mzero[T]
  }

  object FreeMonoidExpr{
    def apply[F[_],X]( i1: X, i2: X)(implicit F: FreeAlg[F,Monoid]): F[X] = { implicit val _ = F.alg[X]
      F.unit(i1) |+| F.unit(i2) |+| mzero[F[X]]
    }
  }

  object ListMonoidExprAdHoc{
    def apply[X](i1: X, i2: X): List[X] = List(i1) |+| List(i2) |+| mzero[List[X]]
  }

  object ListMonoidExprMonolythic{
    implicit val listFree = new FreeAlg[List,Monoid]{
      def alg[X] = Monoid[List[X]]
      def unit[X](x: X) = List(x)
      def foldMap[X,T](f: X => T)(implicit alg: Monoid[T]) = (_: List[X]).foldMap(f)
    }

    def apply[X](i1: X, i2: X): List[X] = FreeMonoidExpr[List,X](i1,i2)
  }


  // Higher kinds

  object StateMonadExpr{
    def apply: State[Int,Boolean] = 
      State.get[Int] flatMap { i => 
        State.put(i+1) flatMap { _ => 
          State.get flatMap { i => 
            State.state[Int,Boolean](i > 0) 
          }
        }
      }
  }

  object WriterMonadExpr{
    def apply: Writer[List[Int],Boolean] = 
      WriterT.tell(List(1)) flatMap { _ => 
        WriterT.tell(List(2)) flatMap { _ => 
          WriterT.writer((List(),true))
        }
      }
  }

  object ReaderMonadExpr{
    def apply: Reader[List[Int],Boolean] = 
      ReaderT.ask[Id,List[Int]] flatMap { l => 
        ReaderT.ask[Id,List[Int]] flatMap { l => 
          Reader(_ => true)
        }
      }
  }



}