
import org.scalatest._

class Lists extends FunSpec with Matchers{

  /**
   * List algebras
   */

  object FAlgebra{

    sealed abstract class ListF[A,L]
    case class NilF[A,L]() extends ListF[A,L]
    case class ConsF[A,L](a: A, tail: L) extends ListF[A,L]

    type ListAlg[A,L] = ListF[A,L] => L
  }

  object ObjectAlgebra{
    trait ListAlg[A,L]{
      def nil(): L
      def cons(head: A, tail: L): L
    }
  }

  /**
   * List languages
   */

  object AdHoc{

    sealed abstract class List[A]
    case class Nil[A]() extends List[A]
    case class Cons[A](a: A, tail: List[A]) extends List[A]
  }

  object ChurchEncoded{

    trait Church[Alg[_]]{
      def apply[L](alg: Alg[L]): L
    }

    import FAlgebra.ListAlg

    type List[A] = Church[ListAlg[A,?]]
  }

  object FixEncoded{

    case class Fix[F[_]](out: F[Fix[F]])

    import FAlgebra.ListF

    type List[A] = Fix[ListF[A,?]]
  }

}


class SafeLists extends FunSpec with Matchers{
  import scalaz.~>

  trait Z
  trait S[N]

  /**
   * List algebras
   */

  object FAlgebraADT{

    sealed abstract class ListF[A,L[_],_]
    case class NilF[A,L[_],N](cont: Z => N) extends ListF[A,L,N]
    case class ConsF[A,L[_],M,N](a: A, tail: L[M], cont: S[M] => N) extends ListF[A,L,N]

    type ListAlg[A,L[_]] = ListF[A,L,?] ~> L
  }

  object FAlgebraGADT{

    sealed abstract class ListF[A,L[_],_]
    case class NilF[A,L[_]]() extends ListF[A,L,Z]
    case class ConsF[A,L[_],M](a: A, tail: L[M]) extends ListF[A,L,S[M]]

    type ListAlg[A,L[_]] = ListF[A,L,?] ~> L
  }

  object ObjectAlgebraADT{
    trait ListAlg[A,L[_]]{
      def nil[N](cont: Z => N): L[N]
      def cons[M,N](head: A, tail: => L[M], cont: S[M] => N): L[N]
    }
  }

  object ObjectAlgebraGADT{
    trait ListAlg[A,L[_]]{
      def nil(): L[Z]
      // needs delayed argument for tail
      def cons[M](head: A, tail: => L[M]): L[S[M]]
    }
  }

  /**
   * List languages
   */
  object AdHocGADT{

    sealed abstract class List[A,_]
    case class Nil[A]() extends List[A,Z]
    case class Cons[A,M](a: A, tail: List[A,M]) extends List[A,S[M]]

    // Head & safe head

    def head[A,N](l: List[A,N]): A = l match {
      case Cons(a,_) => a
      case _ => ???
    }

    def safeHead[A,M](l: List[A,S[M]]): A = l match {
      case Cons(a,_) => a
    }

    def safeHeadIndirectly[A,M](l: List[A,S[M]]): A =
      head(l)

    // List sample

    val nonEmptyList = Cons(1,Cons(2,Nil[Int]()))
    def emptyList[A] = Nil[A]()
    safeHead(nonEmptyList)

    // Doesn't compile
    // safeHead(emptyList[Int])
    // safeHeadIndirectly(emptyList[Int])

  }

  object AdHocADTGen{

    sealed abstract class List[A,_]
    case class Nil[A,N](cont: Z => N) extends List[A,N]
    case class Cons[A,M,N](a: A, tail: List[A,M], cont: S[M] => N) extends List[A,N]
  }

  object ChurchEncoded{

    trait Church[Alg[_[_]],T]{
      def apply[L[_]](alg: Alg[L]): L[T]
    }

    import ObjectAlgebraGADT.ListAlg

    type List[A,N] = Church[ListAlg[A,?[_]],N]

    // head

    def head[A,N](list: List[A,N]): A =
      list[λ[α=>A]](new ListAlg[A,λ[α=>A]]{
        def nil(): A = ???
        def cons[M](head: A, tail: => A): A = head
      })

    def safeHeadIndirectly[A,M](list: List[A,S[M]]): A =
      head(list)

  }

  describe("Church encoded"){
    import ObjectAlgebraGADT.ListAlg, ChurchEncoded._

    val nonEmptyList: List[Int,S[S[Z]]] = new Church[ListAlg[Int,?[_]],S[S[Z]]]{
      def apply[L[_]](alg: ListAlg[Int,L]) =
        alg.cons(1, alg.cons(2, alg.nil()))
    }

    def emptyList[A]: List[A,Z] = new Church[ListAlg[A,?[_]],Z]{
      def apply[L[_]](alg: ListAlg[A,L]) =
        alg.nil()
    }

    it("should work for non-empty list"){
      // safeHeadIndirectly[Int,S[Z]](nonEmptyList)
      safeHeadIndirectly(nonEmptyList)
    }

    it("should work for empty list"){
      an[NotImplementedError] shouldBe thrownBy(head(emptyList[Int]))
      "safeHeadIndirectly(emptyList[Int])" shouldNot compile
    }
  }

  object FixEncoded{

    case class Fix[F[_[_],_],T](out: F[Fix[F,?],T])

    import FAlgebraADT.ListF

    type List[A,N] = Fix[ListF[A,?[_],?],N]
  }

}