package org.hablapps.gist

object EffMTL{

  /** APIs */

  trait Error[P[_]]{
    def raise[A](e: String): P[A]
  }

  trait IO[P[_]]{
    def read(): P[String] 
    def write(msg: String): P[Unit]
  }

  import cats.Monad, cats.Applicative

  /* LOGIC */

  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.syntax.cartesian._
  import cats.data.Kleisli
  import scala.util.{Try, Success, Failure}

  def echo[P[_]: Monad]()(implicit E: Error[P], IO: IO[P]): P[Unit] = 
    IO.read() >>= 
    (checkNotEmpty[P] *> checkNotNumber[P]).run >>= 
    (IO.write _)

  import cats.syntax.applicative._

  def checkNotNumber[P[_]: Applicative](implicit E: Error[P]): Kleisli[P,String,String] =
    Kleisli{ msg => 
      Try(msg.toInt) match {
        case Success(_) => E.raise("is number")
        case _ => msg.pure[P]
      }
    }

  def checkNotEmpty[P[_]: Applicative](implicit E: Error[P]): Kleisli[P,String,String] =
    Kleisli{ msg => 
      if (msg == "") E.raise("empty")
      else msg.pure[P]
    }






}