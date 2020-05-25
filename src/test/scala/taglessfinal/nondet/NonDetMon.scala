package org.hablapps.gist
package nondet
package monadic

// monadic, non-direct style

import cats.Id

abstract class MonadTF[Repr[_]]{
  type Effect[_]

  def pure[A](a: Repr[A]): Repr[Effect[A]]
  def bind[A, B](c: Repr[Effect[A]])(f: Repr[A] => Repr[Effect[B]]): Repr[Effect[B]]

  def map[A, B](c: Repr[Effect[A]])(f: Repr[A] => Repr[B]): Repr[Effect[B]] =
    bind(c)(f andThen pure[B])
}

object MonadTF{
  type Aux[Repr[_], E[_]] = MonadTF[Repr]{ type Effect[x] = E[x] }

  implicit val _IdMonadTF: Aux[Id, List] = new MonadTF[Id]{
    type Effect[x] = List[x]

    def pure[A](a: A) = List(a)
    def bind[A, B](c: List[A])(f: A => List[B]): List[B] = c.flatMap(f)
  }
}


abstract class NonDetetermism[Repr[_]]{
  // alternative, tagged types: type NonDet[x] = List[x]@@ND
  type NonDet[_]

  def fail[A]: Repr[NonDet[A]]
  def choice[A](a: Repr[NonDet[A]], b: Repr[NonDet[A]]): Repr[NonDet[A]]
}

object NonDetetermism{
  type Aux[Repr[_], ND[_]] = NonDetetermism[Repr]{ type NonDet[x] = ND[x] }

  implicit val _Id: Aux[Id, List] = new NonDetetermism[Id]{
    type NonDet[x] = List[x]

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

case class Perm[Repr[_], ND[_]](implicit
  val M: MonadTF.Aux[Repr, ND],
  val ND: NonDetetermism.Aux[Repr, ND],
  val L: Lists[Repr],
  val B: Base[Repr]){

  import ND.NonDet

  def insert[A](a: Repr[A], r: Repr[NonDet[List[A]]]): Repr[NonDet[List[A]]] =
    M.bind(r){ l =>
      ND.choice(
        M.pure(L.cons(a, l)),
        L.`match`(l)(
          ND.fail[List[A]],
          (head, tail) => M.map(insert(a, M.pure(tail)))(L.cons[A](head,_))
        )
      )
    }

  def insert2[A](a: Repr[A], r: Repr[NonDet[List[A]]]): Repr[NonDet[List[A]]] =
    M.bind(r){ l =>
      L.`match`(l)(
        M.pure(L.cons(a, L.nil)),
        (head, tail) =>
          ND.choice(
            M.pure(L.cons(a, L.cons(head, tail))),
            M.map(insert2(a, M.pure(tail)))(L.cons[A](head,_))
          )
      )
    }

  def insert3[A](a: Repr[A], r: Repr[NonDet[List[A]]]): Repr[NonDet[List[A]]] =
    M.bind(r)(
      L.recur[A, NonDet[List[A]]]
        (M.pure(L.cons(a, L.nil)),
        (head, tail) => tailsol =>
          ND.choice(
            M.pure(L.cons(a, L.cons(head, tail))),
            M.map(tailsol)(L.cons[A](head,_))
          )
      ))

  def perm[A](l: Repr[List[A]]): Repr[NonDet[List[A]]] =
    L.foldr[A, NonDet[List[A]]](M.pure(L.nil[A]), insert2[A])(l)
}

import org.scalatest._

class NonDetMonadic extends FunSpec with Matchers{

  describe("NonDeterminism through tagless final with tagless-final monads"){

    val P = Perm[Id, List]

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



