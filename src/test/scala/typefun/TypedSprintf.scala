package org.hablapps.gist
package typefun

import org.scalatest._
import scala.util.Try


// SECTION 4.1 TYPED SPRINTF

trait Read[T]{
  def read(str: String): Option[(T, String)]
}

object Read{

  def apply[A](implicit R: Read[A]): Read[A] = R

  implicit val IntRead = new Read[Int]{
    val IntPrefix = """(\d+)(.*)""".r

    def read(str: String): Option[(Int, String)] = str match {
      case IntPrefix(i, tail) => Some((Integer.parseInt(i), tail))
      case _ => None
    }
  }

  implicit val StrRead = new Read[String]{
    def StrPrefix(prefix: String) = """${prefix}(.*)""".r

    def read(str: String): Option[(String, String)] = {
      val R = StrPrefix(str)
      str match {
        case R(tail) => Some((str, tail))
        case _ => None
      }
    }
  }
}

trait Show[T]{
  def write(t: T): String
}

object Show{

  def apply[A](implicit S: Show[A]): Show[A] = S

  implicit val IntShow = new Show[Int]{
    def write(t: Int): String = t.toString
  }
}

sealed abstract class Fmt
case class Lit(s: String) extends Fmt
case class Val[A: Read: Show]() extends Fmt
case class Cmp[F1 <: Fmt, F2 <: Fmt](f1: F1, f2: F2) extends Fmt

class FmtSpec extends FunSpec with Matchers{

  val int = Val[Int]

  Lit("day"): Lit
  Cmp(Lit("day"), Lit("s")): Cmp[Lit, Lit]
  Cmp(Lit("day"), int): Cmp[Lit, Val[Int]]
  Cmp(int, Cmp(Lit("day"), Lit("s"))): Cmp[Val[Int], Cmp[Lit, Lit]]
}

object NaiveApproach{
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
}

class TPrinterNaiveSpec extends FunSpec with Matchers{
  import NaiveApproach._
  import NaiveApproach.TPrinter.Syntax._

  Lit("hola")(()) shouldBe "hola"
  Val[Int].apply(1) shouldBe "1"
  Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int])).apply(((), ((), 1))) shouldBe
    "hola, number 1"
}

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

class TPrinterSpec extends FunSpec with Matchers{
  import TPrinter.Syntax._

  sprintf(Lit("hola")) shouldBe "hola"
  sprintf(Val[Int]).apply(1) shouldBe "1"
  sprintf(Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int]))).apply(1) shouldBe
    "hola, number 1"
}

trait TParser[F <: Fmt, X]{
  type Out

  def apply(f: F)(cont: X): String => Option[(Out, X)]
}

object TParser{

  type Aux[F <: Fmt, X, _Out] = TParser[F, X]{ type Out = _Out }

  def LitParser[X] = new TParser[Lit, X]{
    type Out = Unit

    def apply(f: Lit)(cont: X): String => Option[(Unit, X)] =
      Read[String].read(_).map{
        case (_, _) => ((), cont)
      }
  }

  def ValParser[T: Read, X] = new TParser[Val[T], X]{
    type Out = T

    def apply(f: Val[T])(cont: X): String => Option[(T, X)] =
      Read[T].read(_).map{
        case (t, tail) => (t, cont)
      }
  }

  def CmpParser[F1 <: Fmt, O1, F2 <: Fmt, O2, X](implicit
      P1: TParser.Aux[F1, O2, O1],
      P2: TParser.Aux[F2, X, O2]) = new TParser[Cmp[F1, F2], X]{
    type Out = O1

    def apply(f: Cmp[F1, F2])(cont: X): String => Option[(O1, X)] =
      s => {
        val P1(f._f1){ s1 =>
        P2(f._f2){ s2 =>
          cont(s1+s2)
        }
      }(s)
  }
}















