package org.hablapps.gist

import org.scalatest._

class EffMTL extends FunSpec with Matchers{

  /** APIs */

  trait Error[P[_],E]{
    def raise[A](e: E): P[A]
  }

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]
  }

  import cats.FlatMap, cats.Applicative

  /* LOGIC */

  import cats.data.NonEmptyList
  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.syntax.cartesian._
  import cats.data.Kleisli
  import scala.util.{Try, Success, Failure}

  type EchoError = NonEmptyList[SingleError]

  sealed abstract class SingleError
  case class IsNumber(i: Int) extends SingleError
  case class TooShort(l: Int) extends SingleError
  case class NotEnoughInput() extends SingleError

  def echo[P[_]: FlatMap: Applicative]()(implicit
    E: Error[P,EchoError], IO: IO[P]): P[Unit] =
    IO.read() >>=
    (checkNotNumber[P] *> checkLength[P]).run >>=
    (IO.write _)

  import cats.syntax.applicative._

  def checkNotNumber[P[_]: Applicative](implicit
    E: Error[P,EchoError]): Kleisli[P,String,String] =
    Kleisli{ msg =>
      Try(msg.toInt) match {
        case Success(i) => E.raise(NonEmptyList.of(IsNumber(i)))
        case _ => msg.pure[P]
      }
    }

  def checkLength[P[_]: Applicative](implicit
    E: Error[P,EchoError]): Kleisli[P,String,String] =
    Kleisli{ msg =>
      if (msg.length <= 2) E.raise(NonEmptyList.of(TooShort(msg.length)))
      else msg.pure[P]
    }

  /* TESTS */

  import org.hablapps.puretest._,
    scalatestImpl._,
    ProgramStateMatchers.Syntax._

  def test[P[_]
    : FlatMap
    : Applicative
    : IO
    : Error[?[_],EchoError]
    : StateTester[?[_],IOState,EchoError]]: Unit = {

    it("Echo with enough input"){
      echo[P]() should from[P](IOState(List("hi"),List())).runWithoutErrors
    }

    it("Echo without input"){
      echo[P]() should from(IOState(List(),List()))
        .failWith(NonEmptyList.of(NotEnoughInput(): SingleError))
    }

    it("Echo with wrong input with single error"){
      echo[P]() should from(IOState(List("999"),List()))
        .failWith(NonEmptyList.of(IsNumber(999): SingleError))
    }

    it("Echo with wrong input with several errors"){
      echo[P]() should from(IOState(List("9"),List()))
        .failWith(NonEmptyList.of(
          IsNumber(999): SingleError,
          TooShort(1)))
    }
  }

  /* INSTANCES */

  /* IO */

  case class IOState(toBeRead: List[String], written: List[String])

  import cats.data.StateT

  type IOAction[A] = StateT[Option,IOState,A]

  import cats.instances.option._

  object IOActionIO extends IO[IOAction]{
    def read(): IOAction[String] = StateT[Option,IOState,String]{
      case IOState(head::tail,w) => Option((IOState(tail,w),head))
      case _ => None
    }

    def write(msg: String): IOAction[Unit] = StateT{
      case IOState(r,w) => Some((IOState(r,msg::w),()))
    }
  }

  /* Error */

  import cats.data.Validated

  def ValidatedError[E] = new Error[Validated[E,?],E]{
    def raise[A](e: E): Validated[E,A] =
      Validated.invalid(e)
  }


}