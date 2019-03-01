package org.hablapps.gist
package typefun
package paper

import scalaz.StateT, scalaz.std.option._

trait TParser[F <: Fmt, X]{
  type Out

  def apply(f: F)(cont: X): StateT[Option, String, Out]
}

object TParser{

  type Aux[F <: Fmt, X, _Out] = TParser[F, X]{ type Out = _Out }

  implicit def LitParser[X] = new TParser[Lit, X]{
    type Out = X

    def apply(f: Lit)(cont: X): StateT[Option, String, X] =
      StateT{ Read.StrRead(f.s).read(_).map{
        case (tail, _) => (tail, cont)
      }}
  }

  implicit def ValParser[T: Read, X] = new TParser[Val[T], X]{
    type Out = (X, T)

    def apply(f: Val[T])(cont: X): StateT[Option, String, (X, T)] =
      StateT{ Read[T].read(_).map{
        case (tail, t) => (tail, (cont, t))
      }}
  }

  implicit def CmpParser[F1 <: Fmt, O1, F2 <: Fmt, O2, X](implicit
      P1: TParser.Aux[F1, X, O1],
      P2: TParser.Aux[F2, O1, O2]) = new TParser[Cmp[F1, F2], X]{
    type Out = O2

    def apply(f: Cmp[F1, F2])(cont: X): StateT[Option, String, Out] =
      P1(f.f1)(cont) flatMap P2(f.f2)
  }

  object Syntax{

    def sscanf[F <: Fmt](f: F)(implicit P: TParser[F, Unit]): StateT[Option, String, P.Out] =
      P(f)(())
  }
}

import org.scalatest._

class TParserSpec extends FunSpec with Matchers{

  import TParser.Syntax._

  sscanf(Lit("hola")).run("hola") shouldBe Some(("", ()))
  sscanf(Lit("hola")).run("hola, mundo!") shouldBe Some((", mundo!", ()))
  sscanf(Val[Int]).run("2345hola") shouldBe Some(("hola", ((), 2345)))
  sscanf(Cmp(Val[Int], Lit("hola"))).run("2345hola") shouldBe Some(("", ((), 2345)))
}

