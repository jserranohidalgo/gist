package org.hablapps.gist
package typefun
package paper

trait TPrinter[F <: Fmt, X]{
  type Out

  def apply(fmt: F)(cont: String => X): Out
}

object TPrinter{

  type Aux[F <: Fmt, X, _Out] = TPrinter[F, X]{ type Out = _Out }

  implicit def LitTPrinter[X] = new TPrinter[Lit, X]{
    type Out = X

    def apply(fmt: Lit)(cont: String => X): X =
      cont(fmt.s)
  }

  implicit def ValTPrinter[A: Read: Show, X] = new TPrinter[Val[A], X]{
    type Out = A => X

    def apply(fmt: Val[A])(cont: String => X): A => X =
      cont compose Show[A].write
  }

  implicit def CmpTPrinter[F1 <: Fmt, O1, F2 <: Fmt, O2, X](implicit
      P2: TPrinter[F2, X]{ type Out = O2 },
      P1: TPrinter[F1, O2]{ type Out = O1 }) =
    new TPrinter[Cmp[F1, F2], X]{
      type Out = O1

      def apply(fmt: Cmp[F1, F2])(cont: String => X): O1 =
        P1(fmt.f1){ s1 =>
          P2(fmt.f2){ s2 =>
            cont(s1 ++ s2): X
          }: O2
        }: O1

    }

  object Syntax{

    def sprintf[F <: Fmt, Out](f: F)(implicit
        P: TPrinter.Aux[F, String, Out]): Out =
      P(f)(identity)

    // implicit class FmtOps[F <: Fmt, _Out](f: F)(implicit
    //     P: TPrinter[F, String]{ type Out = _Out }){
    //   val printf: _Out =
    //     P(f)(identity)
    // }
  }
}

import org.scalatest._

class TPrinterSpec extends FunSpec with Matchers{
  import TPrinter.Syntax._

  sprintf(Lit("hola")) shouldBe "hola"
  sprintf(Val[Int]).apply(1) shouldBe "1"
  sprintf(Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int]))).apply(1) shouldBe
    "hola, number 1"
}
