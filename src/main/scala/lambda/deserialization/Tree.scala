package org.hablapps.gist
package lambda
package deserialization

sealed abstract class Tree
case class Leaf(l: String) extends Tree
case class Node(r: String, c: List[Tree]) extends Tree

object Tree{

  trait Syntax{

    def tr_int(i: Int): Tree =
      Node("Int", List(Leaf(i.toString)))

    def tr_add(i: Tree, j: Tree): Tree =
      Node("Add", List(i, j))

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))

    val tr_tInt: Tree =
      Node("TInt", List())

    def tr_tArr(t1: Tree, t2: Tree): Tree =
      Node("TArr", List(t1, t2))
  }
}
