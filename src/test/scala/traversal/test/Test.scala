package org.hablapps.gist
package traversal
package test

import org.scalatest._

class Test extends FunSpec with Matchers{
  import wrong.Traversal.Syntax._
  import Tree.Syntax._
  
  describe("Inorder for trees"){
    
    it("should work for empty trees"){
      leaf().getAll()(Tree.InOrder[Int]) shouldBe List()
    }

    it("should work for non-empty trees"){
      node(node(Leaf(),1,Leaf()),2,Node(Leaf(),3,Leaf())).getAll()(Tree.InOrder[Int]) shouldBe
        List(1,2,3)
    }
  }
}