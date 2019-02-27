package org.hablapps.gist
package typefun

import org.scalatest._
import scala.util.Try


// SECTION 4.1 TYPED SPRINTF

trait Read[T]{
  def read(s: String): Option[(T, String)]
}

object Read{

  def apply[A](implicit R: Read[A]): Read[A] = R

  implicit val IntRead = new Read[Int]{
    def read(s: String): Option[(Int, String)] =
      Try(Integer.parseInt(s)).toOption map ((_, ""))
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
  trait SPrintf[F <: Fmt]{
    type Args

    def apply(f: F): Args => String
  }

  object SPrintf{

    implicit val LitSPrintf = new SPrintf[Lit]{
      type Args = Unit

      def apply(f: Lit): Unit => String =
        _ => f.s
    }

    implicit def ValSPrintf[A: Read: Show] = new SPrintf[Val[A]]{
      type Args = A

      def apply(f: Val[A]): A => String =
        Show[A].write
    }

    // trait ConcF1[F1, F2]{
    //   type Out
    // }

    // object ConcF1{

    //   def StringConc[F] = new ConcF1[String, F]{
    //     type Out = F
    //   }

    //   def F1Conc[A, F1, F2](implicit S: ConcF1[F1, F2]) = new ConcF1[A => F1, F2]{
    //     type Out = A => S.Out

    //     def split()
    //   }
    // }

    implicit def CmpSPrintf[F1 <: Fmt, A1, F2 <: Fmt, A2](implicit
        P1: SPrintf[F1]{ type Args = A1 },
        P2: SPrintf[F2]{ type Args = A2 }) =
      new SPrintf[Cmp[F1, F2]]{
        type Args = (A1, A2)

        def apply(f: Cmp[F1, F2]): Args => String = {
          case (arg1, arg2) => P1(f.f1)(arg1) ++ P2(f.f2)(arg2)
        }
      }

    object Syntax{

      implicit class FmtOps[F <: Fmt, A](f: F)(implicit P: SPrintf[F]{ type Args = A }){
        def apply(args: A): String =
          P(f)(args)
      }
    }
  }
}

trait SPrintf[F <: Fmt, X]{
  type Out
}

object SPrintf{

  implicit val LitSPrintf = new SPrintf[Lit, X]{
    type Out = X

    def apply(fmt: Lit): (String => X) => X =
      cont => cont(fmt.s)

  }

  implicit def ValSPrintf[A: Read: Show] = new SPrintf[Val[A], X]{
    type Out = A => X

    def apply(fmt: Val[A]): ((A => String) => X) => X =
      cont => cont(fmt.Show[A].write)
  }

  implicit def CmpSPrintf[F1 <: Fmt, O1, F2 <: Fmt, O2, X](implicit
      P2: SPrintf[F2, X]{ type Out = O2 },
      P1: SPrintf[F1, O2]{ type Out = O1 }) =
    new SPrintf[Cmp[F1, F2], X]{
      type Out = O1

      def apply(fmt: Cmp[F1, F2]): (O1 => X) => X =

    }

  object Syntax{

    implicit class FmtOps[F <: Fmt, A](f: F)(implicit P: SPrintf[F]{ type Out = A }){
      val print: A =
        P(f)
    }
  }
}

class SPrintfSpec extends FunSpec with Matchers{
  import SPrintf.Syntax._

  Lit("hola")(()) shouldBe "hola"
  Val[Int].apply(1) shouldBe "1"
  Cmp(Lit("hola, "), Cmp(Lit("number "), Val[Int])).apply(((), ((), 1))) shouldBe
    "hola, number 1"
}
