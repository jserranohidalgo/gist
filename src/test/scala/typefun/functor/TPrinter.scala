package org.hablapps.gist
package typefun
package functor

import scalaz.Functor, scalaz.Reader, scalaz.Id.Id

trait TPrinter[F <: Fmt]{
  type P[_]
  val F: Functor[P]

  def apply(fmt: F): P[String]
}

object TPrinter{

  def apply[F <: Fmt, _P[_]](implicit P: TPrinter[F]{type P[T] = _P[T]}) = P

  type Aux[F <: Fmt, _P[_]] = TPrinter[F]{ type P[x] = _P[x] }

  implicit def LitTPrinter = new TPrinter[Lit]{
    type P[T] = T
    val F = Functor[Id]

    def apply(fmt: Lit): String =
      fmt.s
  }

  implicit def ValTPrinter[A: Read: Show] = new TPrinter[Val[A]]{
    type P[T] = Reader[A, T]
    val F = Functor[Reader[A, ?]]

    def apply(fmt: Val[A]): Reader[A, String] =
      Reader(Show[A].write)
  }

  implicit def CmpTPrinter[F1 <: Fmt, P1[_], F2 <: Fmt, P2[_]](implicit
      P2: TPrinter.Aux[F2, P2],
      P1: TPrinter.Aux[F1, P1]) =
    new TPrinter[Cmp[F1, F2]]{
      type P[T] = P1[P2[T]]
      val F = P1.F.compose(P2.F)

      def apply(fmt: Cmp[F1, F2]): P1[P2[String]] =
        P1.F.map(P1(fmt.f1)){ s1 =>
          P2.F.map(P2(fmt.f2)){ s2 =>
            s1 ++ s2: String
          }: P2[String]
        }: P1[P2[String]]

    }

  object Syntax{

    def sprintf[F <: Fmt](f: F)(implicit
        TP: TPrinter[F]): TP.P[String] =
      TP(f)
  }
}

import org.scalatest._

class TPrinterSpec extends FunSpec with Matchers{
  import TPrinter.Syntax._

  sprintf(Lit("hola")) shouldBe "hola"
  sprintf(Val[Int]).run(1) shouldBe "1"
  sprintf(Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int])))(
    TPrinter.CmpTPrinter[Lit, Id, Cmp[Lit, Val[Int]], Reader[Int, ?]](
      TPrinter.CmpTPrinter[Lit, Id, Val[Int], Reader[Int, ?]](
        TPrinter[Val[Int], Reader[Int, ?]],
        TPrinter[Lit, Id]
      ),
      TPrinter[Lit, Id])).run(1) shouldBe
    "hola, number 1"
}
