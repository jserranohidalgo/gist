package org.hablapps.gist

import scala.language.existentials
import org.scalatest._
import cats.{~>, Eval, Id, MonadState, MonadWriter, Monad}
import cats.arrow.FunctionK
import cats.data.{State, StateT, Writer, WriterT}

/*
The gist mainly illustrates how to implement compositional interpreters of GADTs
using catamorphisms. Since GADTs are built using type constructors, the `fold` 
function to be implemented requires higher-kinded polymorphism. Moreover, 
in some cases we will need natural transformations instead of regular functions. 

You may want to check first a similar gist for regular ADTs: 

https://github.com/hablapps/gist/blob/master/src/test/scala/ADTs.scala
*/
class GADTs extends FlatSpec with Matchers{

  /*
  The GADT to be used as example allow us to represent imperative IO programs,
  made of simple "read" and "write" instructions. 
  */
  sealed abstract class IO[_]
  case object Read extends IO[String]
  case class Write(msg: String) extends IO[Unit]
  case class FlatMap[A, B](p: IO[A], f: A => IO[B]) extends IO[B]
  case class Pure[A](a: A) extends IO[A]

  /*
  This kind of representation allows us to write the following IO programs
  */
  val e1: IO[Unit] = FlatMap(Read, Write)
  
  val e2: IO[String] =
    FlatMap[Unit, String](e1, _ => Read)
  
  val e3: IO[String] =
    FlatMap[String, String](Read, s1 =>
      FlatMap[Unit, String](
        FlatMap[String, Unit](Read, s2 => Write(s1+s2)), _ =>
          Read)
    )

  /*
  In order to run IO programs, converting them to strings, transforming
  them into normal forms, etc., we implement independent functions.
  */

  trait CompositionalInterpreters {
    // Side-effectful interpretation of IO programs
    def run[A](e: IO[A]): A
    // (Approximate) representation of IO programs as strings
    def write[A](io: IO[A]): String
  }

  trait NonCompositionalInterpreters{
    // Reassociate `FlatMap`s to the right
    def reassociate[A](e: IO[A]): IO[A]
  }

  /*
  And here there are some simple tests for some of the implemented functions
  */
  case class TestCompositional(interpreters: CompositionalInterpreters){
    import interpreters._

    write(e1) shouldBe "FlatMap(Read, Write)"
    write(e2) shouldBe "FlatMap(FlatMap(Read, Write), Read)"
    write(e3) shouldBe "FlatMap(Read, FlatMap(FlatMap(Read, Write), Read))"
  }

  case class TestNonCompositional(interpreters: NonCompositionalInterpreters with CompositionalInterpreters){
    import interpreters._

    write(reassociate(e1)) shouldBe
      write(e1)
    
    write(reassociate(e2)) shouldBe
      "FlatMap(Read, FlatMap(Write, Read))"
    
    write(reassociate(e3)) shouldBe
      "FlatMap(Read, FlatMap(Read, FlatMap(Write, Read)))"

    write(reassociate(FlatMap(e3, (s: String) => Read))) shouldBe
      "FlatMap(Read, FlatMap(Read, FlatMap(Write, FlatMap(Read, Read))))"
  }

  /*
  We will first implement these functions using pattern matching. 
  */
  object PatternMatchingInterpreters extends NonCompositionalInterpreters with CompositionalInterpreters{

    def run[A](e: IO[A]): A = e match {
      case Read => scala.io.StdIn.readLine
      case Write(msg) => println(msg)
      case FlatMap(p, next) => run(next(run(p)))
      case Pure(a) => a
    }

    def write[A](io: IO[A]): String = {
      implicit val stringMonoid = new cats.kernel.instances.StringMonoid
      val monad = WriterT.catsDataMonadWriterForWriterT[Id, String]
      import monad._
    
      def aux[A](_io: IO[A]): Writer[String, A] =
        _io match {
          case Read =>
            writer(("Read", ""))
          case Write(_) =>
            tell(s"Write")
          case FlatMap(p, f) =>
            for {
              _ <- tell(s"FlatMap(")
              a <- aux(p)
              _ <- tell(", ")
              b <- aux(f(a))
              _ <- tell(")")
            } yield b
          case Pure(a) =>
            writer((s"Pure($a)", a))
        }

      aux(io).written
    }

    def reassociate[A](e: IO[A]): IO[A] = e match {
      case FlatMap(FlatMap(p1, next1), next2) =>
        reassociate(FlatMap(p1, next1 andThen (FlatMap(_,next2))))
      case FlatMap(p1, next1) =>
        FlatMap(reassociate(p1), next1 andThen reassociate)
      case other => other
    }
  }
    
  "Pattern matching" should "work" in {
    TestCompositional(PatternMatchingInterpreters)
    TestNonCompositional(PatternMatchingInterpreters)
  }

  /*
  This module implements the catamorphism for IO programs. Note that "read" and
  "write" interpretations are normal functions, since these instructions are not 
  parameterised. On the contrary, interpretations for "sequenced" and "pure" programs
  are represented through natural transformations. 
  */
  object HKFold{

    // Type alias for the interpretation of composite IO programs

    case class FlatMapF[M[_],A,B](p: M[A], f: A => M[B])

    object FlatMapF{
      trait NatTrans[M[_]]{
        def apply[A,B](fm: FlatMapF[M,A,B]): M[B]
      }
      
      object NatTrans{
        implicit def apply[P[_]: Monad]: NatTrans[P] = 
          new (NatTrans[P]){ 
            def apply[A,B](fm: FlatMapF[P,A,B]) = 
              Monad[P].flatMap(fm.p)(fm.f)
          }
          // λ[NatTrans[P]]{ fm: FlatMapF[P,_,_] => Monad[P].flatMap(fm.p)(fm.f) }
      }
    }

    // Type alias for the interpretation of pure programs

    object PureF{
      type NatTrans[M[_]] = Id ~> M

      object NatTrans{
        implicit def apply[M[_]: Monad]: NatTrans[M] = 
          new (NatTrans[M]){
            def apply[A](a: A): M[A] = Monad[M].pure(a)
          }
          // Lambda[NatTrans[M]]{ Monad[M].pure _ }
      }
    }

    // Higher-kinded catamorphism

    def fold[F[_]](
        read: => F[String],
        write: String => F[Unit],
        flatMap: FlatMapF.NatTrans[F],
        pure: PureF.NatTrans[F]): IO ~> F = {

      def foldFlatMap[A,B](fm: FlatMap[A,B]) = 
        flatMap(FlatMapF(
          fold(read, write, flatMap, pure)(fm.p),
          (x: A) => fold(read, write, flatMap, pure)(fm.f(x))))

      λ[IO ~> F]{
        case Read => read
        case Write(msg) => write(msg)
        case fm: FlatMap[_,_] => foldFlatMap(fm)
        case Pure(a) => pure(a)
      }
    }
  }

  /*
  We can now implement the compositional interpreters using catamorphisms. Whenever
  possible we create natural transformations for `FlatMap` and `Pure` programs 
  from available monad instances.
  */
  object FoldInterpreters extends CompositionalInterpreters {
    import HKFold._

    def run[A](io: IO[A]): A =
      fold[Id](
        scala.io.StdIn.readLine,
        println,
        FlatMapF.NatTrans[Id],
        FunctionK.id)(io)


    def write[A](io: IO[A]): String = {
      implicit val stringMonoid = new cats.kernel.instances.StringMonoid
      val monad = WriterT.catsDataMonadWriterForWriterT[Id, String]
      import monad._
      
      val FlatMapNatTransForWrite = new FlatMapF.NatTrans[Writer[String,?]]{
        def apply[A,B](fa: FlatMapF[Writer[String,?],A,B]): Writer[String,B] = 
          for {
            _ <- tell(s"FlatMap(")
            a <- fa.p
            _ <- tell(", ")
            b <- fa.f(a)
            _ <- tell(")")
          } yield b
      }

      fold[Writer[String, ?]](
        writer(("Read", "")),
        msg => tell(s"Write"),
        FlatMapNatTransForWrite,
        PureF.NatTrans[Writer[String,?]])(io).written
    }
  }

  "Catamorphisms" should "work" in TestCompositional(FoldInterpreters)

}

object GADTs extends GADTs
