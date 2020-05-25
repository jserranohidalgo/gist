package org.hablapps.gist
package nondet
package adhoc

// http://okmij.org/ftp/tagless-final/nondet-paper.pdf
// http://okmij.org/ftp/tagless-final/nondet-effect.html

abstract class NonDet[Repr[_]]{
  def fail[A]: Repr[A]
  def choice[A](a: Repr[A], b: Repr[A]): Repr[A]

  def run[A](r: Repr[A]): List[A]
}

object NonDet{
  implicit val _List = new NonDet[List]{
    def fail[A] = List()
    def choice[A](a: List[A], b: List[A]) = a ++ b
    def run[A](r: List[A]) = r
  }
}

abstract class Lists[Repr[_]]{
  def nil[A]: Repr[List[A]]
  def cons[A](head: Repr[A], tail: Repr[List[A]]): Repr[List[A]]
  def list[A](l: List[A]): Repr[List[A]]
  def `match`[A, B](l: Repr[List[A]])(
    nil: Repr[B], cons: (Repr[A], Repr[List[A]]) => Repr[B]): Repr[B]
  def recur[A, B](nil: Repr[B],
    cons: (Repr[A], Repr[List[A]]) => Repr[B] => Repr[B])(
    l: Repr[List[A]]): Repr[B]

  def foldr[A, B](nil: Repr[B], cons: (Repr[A], Repr[B]) => Repr[B])(
    l: Repr[List[A]]): Repr[B] =
    recur[A, B](nil, (h, _) => st => cons(h, st))(l)
}

object Lists{

  def combine[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    l1.foldLeft(Nil: List[C])((acc, a) =>
      l2.foldLeft(acc)((acc2, b) =>
        f(a, b) :: acc2))

  implicit val _List = new Lists[List]{
    def nil[A]: List[List[A]] =
      List(List())

    def cons[A](head: List[A], tail: List[List[A]]): List[List[A]] =
      combine(head, tail)(_ :: _)

    def `match`[A, B](l: List[List[A]])(
      nil: List[B], cons: (List[A], List[List[A]]) => List[B]): List[B] =
      l.flatMap{
        case Nil => nil
        case h::t => cons(List(h), List(t))
      }

    def recur[A, B](nil: List[B], cons: (List[A], List[List[A]]) => List[B] => List[B])(
        l: List[List[A]]): List[B] =
      l.foldLeft(Nil: List[B])((acc, la) =>
        la.foldRight((List[A](), nil)){
          case (head, (tail, lb)) =>
            (head::tail, cons(List(head), List(tail))(lb))
        }._2 ++ acc
      )

    def list[A](l: List[A]): List[List[A]] =
      List(l)
  }
}

abstract class Base[Repr[_]]{
  def int(i: Int): Repr[Int]
}

object Base{
  implicit val _List = new Base[List]{
    def int(i: Int): List[Int] = List(i)
  }
}

case class Perm[Repr[_]](implicit
  val ND: NonDet[Repr],
  val L: Lists[Repr],
  val B: Base[Repr]){

  def insert[A](a: Repr[A], l: Repr[List[A]]): Repr[List[A]] =
    L.recur[A, List[A]](
      L.cons(a, L.nil),
      (head, tail) => tailsol =>
        ND.choice(
          L.cons(a, L.cons(head, tail)),
          L.cons(head, tailsol)))(l)

  def insert2[A](a: Repr[A], l: Repr[List[A]]): Repr[List[A]] =
    L.`match`(l)(
      L.cons(a, L.nil),
      (head, tail) =>
        ND.choice(
          L.cons(a, L.cons(head, tail)),
          L.cons(head, insert2(a, tail))))

  def perm[A](l: Repr[List[A]]): Repr[List[A]] =
    L.foldr(L.nil[A], insert2[A])(l)

  def perm[A](l: List[A]): List[List[A]] =
    ND.run(perm(L.list(l)))

}

import org.scalatest._

class NonDetSpec extends FunSpec with Matchers{

  describe("NonDeterminism through tagless final without monads"){

    val P = Perm[List]

    it("works 1"){
      val test: List[List[Int]] =
        P.perm(P.L.cons(P.B.int(1), P.L.cons(P.B.int(2), P.L.cons(P.B.int(3), P.L.nil))))

      test.toSet shouldBe Set(
        List(1, 3, 2),
        List(1, 2, 3),
        List(3, 2, 1),
        List(3, 1, 2),
        List(2, 3, 1),
        List(2, 1, 3))
    }

    it("works 2"){
      P.perm(List(1,2,3)).toSet shouldBe Set(
        List(1, 3, 2),
        List(1, 2, 3),
        List(3, 2, 1),
        List(3, 1, 2),
        List(2, 3, 1),
        List(2, 1, 3))
    }
  }
}



