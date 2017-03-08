package org.hablapps.gist

import org.scalatest._

class MonadMacro extends FunSpec with Matchers{
  import cats.Monad

  def test[P[_]: Monad](i: Int): P[Int] = monad[P,Int] { 
    i + 1
  }

  describe("Simple pure translation"){
    

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(2)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 2
    }
  }

  def test2[P[_]: Monad](i: Int): P[Int] = monad[P,Int] { 
    val j: Int = i 
    j+1
  }

  describe("One step flatMap"){
    

    it("should work with Option"){
      import cats.instances.option._
      test2[Option](2) shouldBe Some(3)
    }

    it("should work with Id"){
      import cats.Id
      test2[Id](2) shouldBe 3
    }
  }


}