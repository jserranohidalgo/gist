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

  object IO{
    sealed abstract class Error
    case class NotEnoughInput() extends Error
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
    (checkNotNumber[P] *> checkLength[P](3)).run >>=
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

  def checkLength[P[_]: Applicative](min: Int)(implicit
    E: Error[P,EchoError]): Kleisli[P,String,String] =
    Kleisli{ msg =>
      if (msg.length < min) E.raise(NonEmptyList.of(TooShort(msg.length)))
      else msg.pure[P]
    }

  /* TESTS */

  import org.hablapps.puretest._,
    scalatestImpl._

  def testEcho[P[_]
    : FlatMap
    : Applicative
    : IO
    : Error[?[_],EchoError]
    : StateTester[?[_],IOState,EchoError]]: Unit = {

    import ProgramStateMatchers.Syntax._

    it("Echo with enough input"){
      echo[P]() should from(IOState(List("hi"),List()))
        .runWithoutErrors
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

  def testCheckLength[P[_]
    : Applicative
    : Error[?[_],EchoError]
    : Tester[?[_],EchoError]]: Unit = {

    import ProgramMatchers.Syntax._

    it("Right length"){
      checkLength[P](3).apply("999") should runWithoutErrors
      checkLength[P](3).apply("999") should beEqualTo("999")
    }

    it("Wrong length"){
      checkLength(3).apply("3") should failWith(NonEmptyList.of(TooShort(1)): EchoError)
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

  implicit def ValidatedError[E] = new Error[Validated[E,?],E]{
    def raise[A](e: E): Validated[E,A] =
      Validated.invalid(e)
  }

  describe("Validated for checking length"){
    testCheckLength[Validated[EchoError,?]]
  }

  /* Composition */


  /* Compound instance */ 




}