package org.hablapps.gist
package traversal
package test

import org.scalatest._

class Test extends FunSpec with Matchers{
  import shapeless._
  import concrete.{ListN, Nil, ::}
  
  describe("Inorder for trees (typelevel)"){
    val InOrder = new Tree.InOrder[Int]
    import InOrder._

    it("should work for empty list"){
      // val content: ListN.Aux[Int,Succ[_0]] = InOrder(Leaf[Int]()).getAll
      val content: ListN.Aux[Int,_0] = InOrder(Leaf[Int]()).getAll
      content shouldBe Nil()
    }

    it("should work for non-nempty list"){
      val content: ListN.Aux[Int, Succ[_0]] = 
        InOrder(Node(Leaf[Int](),1,Leaf[Int]())).getAll 
        
      // val content: ListN.Aux[Int, Succ[Succ[Succ[_0]]]] = 
        // InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, 
        //   Node(Leaf[Int](),3,Leaf[Int]()))).getAll 
      content shouldBe ::(1,::(2,::(3,Nil)))
    }
  }


  // describe("Inorder for trees"){
  //   import Tree.Syntax._
  //   import wrong.Traversal.Syntax._
    
  //   it("should work for empty trees"){
  //     leaf().getAll()(Tree.InOrder[Int]) shouldBe List()
  //   }

  //   it("should work for non-empty trees"){
  //     node(node(Leaf(),1,Leaf()),2,Node(Leaf(),3,Leaf())).getAll()(Tree.InOrder[Int]) shouldBe
  //       List(1,2,3)
  //   }
  // }
}