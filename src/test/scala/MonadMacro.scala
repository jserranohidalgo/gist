package org.hablapps.gist

import org.scalatest._


class MonadMacro extends FunSpec with Matchers with Inside{
  import cats.Monad


  describe("Simple pure translation"){

    def test[P[_]: Monad](i: Int): P[Int] = monad{
      i + 1
    }

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(3)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 3
    }
  }


  describe("Several simple flatMaps"){

    def test[P[_]: Monad](i: Int): P[Int] = monad{
      val s: String = "2"
      val j: Int = s.length + i
      j+1
    }

    // Option

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(4)
    }

    // Id

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 4
    }

    // Program

    abstract class Program[_]
    case class Returns[A](a: A) extends Program[A]
    case class DoAndThen[A,B](a: Program[A],
      f: A => Program[B]) extends Program[B]

    object Program{
      implicit val M = new Monad[Program]{
        def pure[A](a: A) = Returns(a)
        def flatMap[A,B](p: Program[A])(f: A => Program[B]) =
          DoAndThen(p,f)
        def tailRecM[A,B](a: A)(f: A => Program[Either[A,B]]) = ???
      }
    }

    it("should work with Program"){
      inside(test[Program](2)) {
        case DoAndThen(Returns("2"), f) =>
          inside(f("2")) {
            case DoAndThen(Returns(3), g) =>
              g(3) shouldBe Returns(4)
          }
      }
    }
  }

  describe("Simple example with .run"){

    import monad._

    def test[P[_]: Monad](p1: P[String], p2: P[Int]): P[Int] = monad{
      val i: String = p1.run
      val j: Int = p2.run
      i.length + j
    }

    import cats.instances.option._

    it("should work with Option"){
      test[Option](Some("ab"),Some(1)) shouldBe Some(3)
    }

  }

  describe("Sime example with additional APIs"){
    import cats.Id

    trait IO[P[_]]{
      def read(): P[String]
      def write(msg: String): P[Unit]
    }

    object IO{
      object Syntax{
        def read[P[_]]()(implicit IO: IO[P]) = IO.read()
        def write[P[_]](msg: String)(implicit IO: IO[P]) = IO.write(msg)
      }

      implicit object IOId extends IO[Id]{
        def read() = scala.io.StdIn.readLine()
        def write(msg: String) = println(msg)
      }
    }

    import IO.Syntax._, monad._

    // Doesn't really work now
    def test1[P[_]: Monad: IO](): P[String] = monad{
      val msg: String = read[Id]()
      val _  = write[Id](msg)
      msg
    }

    def test[P[_]: Monad: IO](): P[String] = monad{
      val msg: String = read().run
      val _  = write(msg).run
      msg
    }

    it("should work with IO"){
      test[Id]() shouldBe "hi!"
    }
  }


}