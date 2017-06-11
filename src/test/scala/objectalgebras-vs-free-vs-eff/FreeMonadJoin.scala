
package org.hablapps.gist

import org.scalatest._
import cats._
import cats.implicits._
import cats.data.State
import cats.free.Free
import scala.util.Try

import org.scalatest._

object FreeAlgebraApproach extends FreeAlgebraApproach

class FreeAlgebraApproach extends FunSpec with Matchers with Inside{
    
  object AdHoc{
    sealed abstract class IOInst[_]
    case class Read[X](unit: Unit,   cont: String => X) extends IOInst[X]
    case class Write[X](msg: String, cont: Unit => X) extends IOInst[X]
    
    // μY ((1,Y^I) + (O,Y^1) + X)
    // μY (   Y^I  + (O,Y  ) + X)
    case class IOProgram[X](out: Read[IOProgram[X]] Either 
                                 Write[IOProgram[X]] Either 
                                 X)
  }
  
  // IO grammar

  sealed abstract class IOF[_]
  case class Read[X](cont: String => X) extends IOF[X]
  case class Write[X](msg: String, cont: X) extends IOF[X]

  object IOF{
    type Alg[X] = IOF[X] => X

    implicit val IOF_Functor = new Functor[IOF]{
      def map[X,Y](iof: IOF[X])(f: X => Y): IOF[Y] = 
        iof match {
          case Read(cont) => Read(cont andThen f)
          case Write(msg, cont) => Write(msg, f(cont))
        }
    }
  }

  // Free algebra of algebra F[X] => X

  sealed abstract class FreeAlg[F[_],_]
  case class Pure[F[_],X](x: X) extends FreeAlg[F,X]
  case class Join[F[_],X](cont: F[FreeAlg[F,X]]) extends FreeAlg[F,X]

  object FreeAlg{

    // Every free algebra induces a monad
    implicit def FreeAlgMonad[F[_]: Functor] = new Monad[FreeAlg[F,?]]{
      def pure[X](x: X) = Pure(x)
      def flatMap[X,Y](fa: FreeAlg[F,X])(cont: X => FreeAlg[F,Y]) = 
        fa match {
          case Pure(x) => cont(x)
          case Join(fx) => Join(Functor[F].map(fx)(flatMap(_)(cont)))
        }

      // TBD
      def tailRecM[A, B](a: A)(f: A => FreeAlg[F,Either[A,B]]): FreeAlg[F,B] = ???
    }

    // Auxiliary ops
    def lift[F[_]: Functor,X](fx: F[X]): FreeAlg[F,X] = 
      Join(Functor[F].map(fx)(Pure.apply))

    // Interpreter
    import cats.syntax.flatMap._, cats.syntax.functor._

    def interpreter[F[_]: Functor, P[_]: Monad](alg: F ~> P): FreeAlg[F,?] ~> P = 
      λ[FreeAlg[F,?] ~> P]{
        case Pure(x) => 
          Monad[P].pure(x)
        case Join(inst) =>  
          alg(inst).map(interpreter(alg).apply).flatten
      }
  }

  // IO language as Free IO algebra
  
  type IOProgram[T] = FreeAlg[IOF,T]

  object IOProgram{
    object Syntax{
      def read(): IOProgram[String] = 
        FreeAlg.lift(Read(identity))
      def write(msg: String): IOProgram[Unit] = 
        FreeAlg.lift(Write(msg, ()))
    }
  }

  // Sample IO Programs
  
  import IOProgram.Syntax._, cats.syntax.flatMap, cats.syntax.functor._

  def echo(): IOProgram[String] = for{
    msg <- read()
    _ <- write(msg)
  } yield msg

  def readInt(): IOProgram[Int] = 
    read() map (_.toInt) 

  def sum(): IOProgram[Int] = for {
    i1 <- readInt()
    i2 <- readInt()
    sum = i1 + i2
    _  <- write(sum.toString)
  } yield sum

  describe("IO programs built from the free io algebra"){
    import IOProgram.Syntax._, cats.syntax.flatMap, cats.syntax.functor._

    it("works for X = Unit"){
      write("hi") shouldBe 
        Join[IOF,Unit](Write("hi", Pure[IOF,Unit](())))
    }

    it("works for X = String"){
      // read() shouldBe Read(i1 => Pure(i1))
      // read() shouldBe Join(Read(i1 => Pure(i1)))  
      inside(read()) {
        case Join(Read(cont)) => cont("") shouldBe Pure("")
      }

      // echo() shouldBe Read(i1 => Write(i1, Pure(i1)))
      inside(echo()){
        case Join(Read(cont)) => 
          cont("hi") shouldBe 
            Join[IOF,String](Write("hi",Pure[IOF,String]("hi")))
      }
    }
  }

  // Console-based interpretation of IO Programs

  import cats.Id

  val ConsoleIOEffect = λ[IOF ~> Id]{
    case Read(cont) => cont(scala.io.StdIn.readLine())
    case Write(msg,x) => println(msg); x
  }

  // State-based interpretation of IO Programs

  import cats.data.State

  case class IOState(in: List[String], out: List[String])

  type IOAction[T]=State[IOState,T]

  val IOActionIOEffect = λ[IOF ~> IOAction]{
    case Read(cont) => State{
      case IOState(msg::reads,written) => 
        (IOState(reads,written), cont(msg))
    }
    case Write(msg,x) => State{
      case IOState(reads, written) => 
        (IOState(reads, msg::written),x)
    }
  }

  describe("State-based echo"){
    it("should work"){
      FreeAlg.interpreter(IOActionIOEffect).apply(echo())                   
        .run(IOState(List("hi"),List())) 
        .value shouldBe (IOState(List(),List("hi")),"hi")
    }
  }

}