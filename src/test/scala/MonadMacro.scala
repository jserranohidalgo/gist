package org.hablapps.gist

import org.scalatest._

class MonadMacro extends FunSpec with Matchers{
  import cats.Monad

  describe("Simple pure translation"){
    
    def test[P[_]: Monad](i: Int): P[Int] = monad { 
      i 
    }

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(2)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 2
    }
  }

  describe("One step flatMap"){
    
    def test[P[_]: Monad](i: Int): P[Int] = monad { 
      val j: Int = i 
      j+1
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


}