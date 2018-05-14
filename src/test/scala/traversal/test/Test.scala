package org.hablapps.gist
package traversal
package test

import org.scalatest._

class Test extends FunSpec with Matchers{
  import shapeless._
  import concrete.{ListN, Nil, ::}

  val TreeOfInt = Tree.Of[Int]
  import TreeOfInt.InOrder

  describe("GetAll for InOrder traversals"){

    it("should return empty list for empty trees"){
      val content: ListN.Aux[Int,_0] =
        InOrder(Leaf[Int]()).getAll

      content shouldBe Nil()
    }

    it("should return non-nempty list for non-empty trees"){
      val content: ListN.Aux[Int, Succ[Succ[Succ[_0]]]] =
        InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]())))
          .getAll

      content shouldBe ::(1,::(2,::(3,Nil[Int]())))
    }

    it("should not compile when size expectations are wrong"){
      """val content: ListN.Aux[Int,Succ[_0]] =
        InOrder(Leaf[Int]()).getAll""" shouldNot compile

      """val content: ListN.Aux[Int, Succ[Succ[_0]]] =
           InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]())))
             .getAll""" shouldNot compile
    }
  }

  describe("PutAll for InOrder traversals"){

    it("should return the empty tree for updates of empty trees"){
      val out: Leaf[Int] =
        InOrder(Leaf[Int]()).putAll(Nil[Int]())

      out shouldBe Leaf()
    }

    it("should return an updated tree for updates on non-empty trees"){
      val in: Node[Node[Leaf[Int],Int,Leaf[Int]], Int, Node[Leaf[Int],Int,Leaf[Int]]] =
        Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]()))

      val out: Node[Node[Leaf[Int],Int,Leaf[Int]], Int, Node[Leaf[Int],Int,Leaf[Int]]] =
        InOrder(in).putAll(::(3,::(4,::(5,Nil[Int]()))))

      out shouldBe
        Node(Node(Leaf[Int](),3,Leaf[Int]()), 4, Node(Leaf[Int](),5,Leaf[Int]()))
    }

    it("should not compile when input size expectations are wrong"){
      """InOrder(Leaf[Int]()).putAll(::(1,Nil[Int]()))""" shouldNot compile

      """InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]())))
        .putAll(::(4,::(5,Nil[Int]())))""" shouldNot compile

      """InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]())))
        .putAll(::(2,::(3,::(4,::(5,Nil[Int]())))))""" shouldNot compile
    }

    it("should not compile when output size expectations are wrong"){
      """val out: Node[Leaf[Int],Int,Leaf[Int]] = InOrder(Leaf[Int]()).putAll(Nil[Int]())""" shouldNot compile

      """val out: Node[Leaf[Int], Int, Node[Leaf[Int],Int,Leaf[Int]]] =
        InOrder(Node(Node(Leaf[Int](),1,Leaf[Int]()), 2, Node(Leaf[Int](),3,Leaf[Int]())))
          .putAll(::(3,::(4,::(5,Nil[Int]()))))""" shouldNot compile
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