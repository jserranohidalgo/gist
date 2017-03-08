package org.hablapps.gist

import org.scalatest._

class MonadMacro extends FunSpec with Matchers{
  import cats.Monad


  describe("Simple pure translation"){
    
    def test[P[_]: Monad](i: Int): P[Int] = monad[P,Int] { 
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

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(4)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 4
    }
  
    abstract class Program[_]
    case class Returns[A](a: A) extends Program[A]
    case class DoAndThen[A,B](a: Program[A],
      f: A => Program[B]) extends Program[B]

    implicit val M = new Monad[Program]{
      def pure[A](a: A) = Returns(a)
      def flatMap[A,B](p: Program[A])(f: A => Program[B]) = 
        DoAndThen(p,f)
      def tailRecM[A,B](a: A)(f: A => Program[Either[A,B]]) = ???
    }

    it("should work with Program"){
      test[Program](2) should matchPattern {
        case DoAndThen(Returns("2"), f) =>
      }
    }
  }


}