package org.hablapps.gist
package typefun
package naive

trait TPrinter[F <: Fmt]{
  type Args

  def apply(f: F): Args => String
}

object TPrinter{

  implicit val LitTPrinter = new TPrinter[Lit]{
    type Args = Unit

    def apply(f: Lit): Unit => String =
      _ => f.s
  }

  implicit def ValTPrinter[A: Read: Show] = new TPrinter[Val[A]]{
    type Args = A

    def apply(f: Val[A]): A => String =
      Show[A].write
  }

  implicit def CmpTPrinter[F1 <: Fmt, A1, F2 <: Fmt, A2](implicit
      P1: TPrinter[F1]{ type Args = A1 },
      P2: TPrinter[F2]{ type Args = A2 }) =
    new TPrinter[Cmp[F1, F2]]{
      type Args = (A1, A2)

      def apply(f: Cmp[F1, F2]): Args => String = {
        case (arg1, arg2) => P1(f.f1)(arg1) ++ P2(f.f2)(arg2)
      }
    }

  object Syntax{

    implicit class FmtOps[F <: Fmt, A](f: F)(implicit P: TPrinter[F]{ type Args = A }){
      def apply(args: A): String =
        P(f)(args)
    }
  }
}

import org.scalatest._

class TPrinterNaiveSpec extends FunSpec with Matchers{
  import TPrinter.Syntax._

  Lit("hola")(()) shouldBe "hola"
  Val[Int].apply(1) shouldBe "1"
  Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int])).apply(((), ((), 1))) shouldBe
    "hola, number 1"
}
