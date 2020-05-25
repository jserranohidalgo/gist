package org.hablapps.gist
package nondet
package monadic

// monadic, non-direct style

import cats.Id

abstract class NonDet[Repr[_]]{
  type NonDet[_]

  def pure[A](a: Repr[A]): Repr[List[A]]
  def map[A, B](c: Repr[List[A]])(f: Repr[A] => Repr[B]): Repr[List[B]]
  def bind[A, B](c: Repr[List[A]])(f: Repr[A] => Repr[List[B]]): Repr[List[B]]

  def fail[A]: Repr[List[A]]
  def choice[A](a: Repr[List[A]], b: Repr[List[A]]): Repr[List[A]]
}

object NonDet{
  implicit val _Id = new NonDet[Id]{
    def pure[A](a: A) = List(a)
    def map[A, B](c: List[A])(f: A => B) = c.map(f)
    def bind[A, B](c: List[A])(f: A => List[B]): List[B] = c.flatMap(f)
    def fail[A]: List[A] = List()
    def choice[A](a: List[A], b: List[A]) = a ++ b
  }
}

abstract class Lists[Repr[_]]{
  def nil[A]: Repr[List[A]]
  def cons[A](head: Repr[A], tail: Repr[List[A]]): Repr[List[A]]
  def recur[A, B](nil: Repr[B],
    cons: (Repr[A], Repr[List[A]]) => Repr[B] => Repr[B])(
    l: Repr[List[A]]): Repr[B]
  def list[A](l: List[A]): Repr[List[A]]

  def foldr[A, B](nil: Repr[B], cons: (Repr[A], Repr[B]) => Repr[B])(
    l: Repr[List[A]]): Repr[B] =
    recur[A, B](nil, (h, _) => st => cons(h, st))(l)
    // TBD: delayed argument to avoid recursion
  def `match`[A, B](l: Repr[List[A]])(
    nil: Repr[B], cons: (Repr[A], Repr[List[A]]) => Repr[B]): Repr[B] =
    recur[A, B](nil, (head, tail) => _ => cons(head, tail))(l)
}

object Lists{

  implicit val _Id = new Lists[Id]{
    def nil[A]: List[A] = Nil
    def cons[A](head: A, tail: List[A]): List[A] = head :: tail
    def recur[A, B](nil: B, cons: (A, List[A]) => B => B)(l: List[A]): B =
      l.foldRight((List[A](), nil)){
          case (head, (tail, lb)) =>
            (head::tail, cons(head, tail)(lb))
        }._2
    def list[A](l: List[A]): List[A] = l
  }
}

abstract class Base[Repr[_]]{
  def int(i: Int): Repr[Int]
}

object Base{
  implicit val _Id = new Base[Id]{
    def int(i: Int): Int = i
  }
}

case class Perm[Repr[_]](implicit
  val ND: NonDet[Repr],
  val L: Lists[Repr],
  val B: Base[Repr]){

  def insert[A](a: Repr[A], r: Repr[List[List[A]]]): Repr[List[List[A]]] =
    ND.bind(r){ l =>
      ND.choice(
        ND.pure(L.cons(a, l)),
        L.`match`(l)(
          ND.fail[List[A]],
          (head, tail) => ND.map(insert(a, ND.pure(tail)))(L.cons[A](head,_))
        )
      )
    }

  def insert2[A](a: Repr[A], r: Repr[List[List[A]]]): Repr[List[List[A]]] =
    ND.bind(r){ l =>
      L.`match`(l)(
        ND.pure(L.cons(a, L.nil)),
        (head, tail) =>
          ND.choice(
            ND.pure(L.cons(a, L.cons(head, tail))),
            ND.map(insert2(a, ND.pure(tail)))(L.cons[A](head,_))
          )
      )
    }

  def insert3[A](a: Repr[A], r: Repr[List[List[A]]]): Repr[List[List[A]]] =
    ND.bind(r)(
      L.recur[A, List[List[A]]]
        (ND.pure(L.cons(a, L.nil)),
        (head, tail) => tailsol =>
          ND.choice(
            ND.pure(L.cons(a, L.cons(head, tail))),
            ND.map(tailsol)(L.cons[A](head,_))
          )
      ))

  def perm[A](l: Repr[List[A]]): Repr[List[List[A]]] =
    L.foldr[A, List[List[A]]](ND.pure(L.nil[A]), insert3[A])(l)
}

import org.scalatest._

class NonDetSpec extends FunSpec with Matchers{

  describe("NonDeterminism through tagless final without monads"){

    val P = Perm[Id]

    it("works 1"){
      val test: List[List[Int]] =
        P.perm(P.L.cons(P.B.int(1), P.L.cons(P.B.int(2), P.L.cons(P.B.int(3), P.L.nil))))

      test shouldBe List(
        List(1, 2, 3),
        List(2, 1, 3),
        List(2, 3, 1),
        List(1, 3, 2),
        List(3, 1, 2),
        List(3, 2, 1))
    }

    it("works 2"){
      P.perm(List(1,2,3)) shouldBe List(
        List(1, 2, 3),
        List(2, 1, 3),
        List(2, 3, 1),
        List(1, 3, 2),
        List(3, 1, 2),
        List(3, 2, 1))
    }
  }
}



