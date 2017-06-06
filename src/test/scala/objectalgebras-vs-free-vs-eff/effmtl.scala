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
    def apply[P[_]](implicit IO: IO[P]) = IO

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
      echo[P]() should from(IOState(List("hii"),List()))
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
          IsNumber(9): SingleError,
          TooShort(1)))
    }
  }

  def testPlainIO[P[_]
    : IO
    : FlatMap 
    : StateTester[?[_],IOState,IO.Error]]: Unit = {
      import ProgramStateMatchers.Syntax._
      import cats.syntax.flatMap._

    it("should work with right IO state with simple IO instruction"){
      IO[P].read() should from(IOState(List("hi"),List()))
        .beEqualTo("hi")
    }

    it("shoud work with monadic programs"){
      (IO[P].read() >>= IO[P].write) should from(IOState(List("hi"),List()))
        .runWithoutErrors
    }

    it("should fail with insufficient input"){
      IO[P].read() should from(IOState(List(),List()))
        .failWith(IO.NotEnoughInput(): IO.Error)
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
  import cats.instances.either._

  type IOAction[A] = StateT[Either[IO.Error,?],IOState,A]

  implicit object IOActionIO extends IO[IOAction]{

    def read(): IOAction[String] = StateT[Either[IO.Error,?],IOState,String]{
      case IOState(head::tail,w) => 
        Right((IOState(tail,w),head))
      case _ =>
        Left(IO.NotEnoughInput())
    }

    def write(msg: String): IOAction[Unit] = StateT{
      case IOState(r,w) => 
        Right((IOState(r,msg::w),()))
    }
  }

  describe("Test simple fail-fast IO programs"){
    testPlainIO[IOAction]
  }

  /* Error */

  import cats.data.Validated

  implicit def ValidatedError[E] = new Error[Validated[E,?],E]{
    def raise[A](e: E): Validated[E,A] =
      Validated.invalid(e)
  }

  describe("Test Validated programs"){
    testCheckLength[Validated[EchoError,?]]

    import ProgramMatchers.Syntax._

    it("should catch both errors"){
      (checkLength[Validated[EchoError,?]](3) *> 
       checkNotNumber[Validated[EchoError,?]]).run("9") should 
        failWith[Validated[EchoError,?]](
          NonEmptyList.of(TooShort(1),IsNumber(9)): EchoError)
    }
  }

  /* Compound instance */ 

  import cats.data.State
  import Validated._

  type IOValidatedAction[A] = State[IOState,Validated[EchoError,A]]

  implicit object IOValidatedActionIO extends IO[IOValidatedAction]{

    def read(): IOValidatedAction[String] = State{
      case IOState(head::tail,w) => 
        (IOState(tail,w),valid(head))
      case st =>
        (st, invalid(NonEmptyList.of(NotEnoughInput()): EchoError))
    }

    def write(msg: String): IOValidatedAction[Unit] = State{
      case IOState(r,w) => 
        (IOState(r,msg::w),valid(()))
    }
  }

  implicit object IOValidatedActionError extends Error[IOValidatedAction,EchoError]{
    def raise[A](e: EchoError) = State{
      st => (st, invalid(e))
    }
  }

  describe("Test echo programs with validation and io effects"){

    implicit def StateTStateTester[E, F[_]: Tester[?[_], E], S] =
      new StateTester[λ[t=>State[S, F[t]]], S, E] {
        def apply(state: S) = new Tester[λ[t=>State[S, F[t]]], E]{
          def apply[T](s: State[S, F[T]]): Either[E, T] =
            Tester[F, E].apply(s.runA(state).value)
        }
      }

    testEcho[IOValidatedAction](StateValidatedMonad[NonEmptyList[SingleError],IOState],
      StateValidatedMonad[NonEmptyList[SingleError],IOState],
      IOValidatedActionIO,
      IOValidatedActionError,
      implicitly)
  }

}