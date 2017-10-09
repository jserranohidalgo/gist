package org.hablapps.gist

import scalaz._, Scalaz._

object ComposeInterpretations{

  trait MonadIO[P[_]]{
    def point[A](a: A): P[A]
    def bind[A,B](p: P[A])(f: A => P[B]): P[B]
    def read(): P[String]
    def write(s: String): P[Unit]
  }

  implicit def compose[P[_],Q[_]](implicit P: MonadIO[P], Q: MonadIO[Q]) =
    new MonadIO[λ[α => (P[α], Q[α])]]{
      def point[A](a: A): (P[A],Q[A]) = (P.point(a), Q.point(a))
      def read(): (P[String],Q[String]) = (P.read(),Q.read())
      def write(s: String): (P[Unit],Q[Unit]) = (P.write(s),Q.write(s))
      def bind[A,B](p: (P[A],Q[A]))(f: A => (P[B],Q[B])): (P[B],Q[B]) =
        (P.bind(p._1)(f andThen (_._1)),
         Q.bind(p._2)(f andThen (_._2)))
    }

  def echo[P[_]]()(implicit M: MonadIO[P]): P[String] =
    M.bind(M.read()){ msg =>
      M.bind(M.write(msg)){ _ =>
        M.point(msg)
      }
    }

}
