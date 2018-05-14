package org.hablapps.gist

trait Product[x[_,_]]{
  def outl[A,B](p: A x B): A
  def outr[A,B](p: A x B): B

  def split[A,B1,B2](f1: A => B1, f2: A => B2): A => B1 x B2

  // Derived ops

  def bimap[A1,A2,B1,B2](f1: A1 => B1, f2: A2 => B2): A1 x A2 => B1 x B2 =
    split[A1 x A2, B1, B2](f1 compose outl, f2 compose outr)

  def create[A,B](a: A, b: B): A x B =
    split[Unit,A,B](_ => a, _ => b)(())
}

object Product extends ProductLaws with ProductSyntax with ProductInstances

trait ProductLaws{

  trait Laws[x[_,_]]{
    implicit val P: Product[x]
    import Product.Syntax._

    // Computation laws

    def cmp1[A,B1,B2](f1: A => B1, f2: A => B2)(a: A): Boolean =
      (outl compose f1 ^ f2)(a) == f1(a)

    def cmp2[A,B1,B2](f1: A => B1, f2: A => B2)(a: A): Boolean =
      (outr compose f1 ^ f2)(a) == f2(a)

    // Reflection law

    def refl[B1,B2](p: B1 x B2): Boolean =
      (outl ^ outr[x,B1,B2]).apply(p) == p

    // Fusion law

    def fusion[A1,A2,B1,B2](h: A1 => A2, f1: A2 => B1, f2: A2 => B2)(a1: A1): Boolean =
      ((f1 ^ f2) compose h)(a1) ==
      ((f1 compose h) ^ (f2 compose h)).apply(a1)

    // Functor fusion law

    def functorFusion[A,B1,B2,C1,C2](f1: A => B1, f2: A => B2, g1: B1 => C1, g2: B2 => C2)(a: A): Boolean =
      (g1 x g2 compose f1 ^ f2)(a) ==
      ((g1 compose f1) ^ (g2 compose f2)).apply(a)
  }
}

trait ProductSyntax{

  object Syntax{

    def outl[x[_,_],B1,B2](implicit P: Product[x]): B1 x B2 => B1 =
      P.outl[B1,B2](_)

    def outr[x[_,_],B1,B2](implicit P: Product[x]): B1 x B2 => B2 =
      P.outr[B1,B2](_)

    implicit class PrSyntax1[A](a: A){
      def x[x[_,_],B](b: B)(implicit Poduct: Product[x]): A x B =
        Poduct.create(a,b)
    }

    implicit class PrSyntax2[A1,B1](f1: A1 => B1){
      def ^[x[_,_],B2](f2: A1 => B2)(implicit P: Product[x]): A1 => B1 x B2 =
        P.split(f1,f2)
      def x[x[_,_],A2,B2](f2: A2 => B2)(implicit P: Product[x]): A1 x A2 => B1 x B2 =
        P.bimap(f1,f2)
    }
  }
}


trait ProductInstances{

  implicit val tuple2Product = new Product[Tuple2]{
    def outl[A,B](p: (A,B)): A = p._1
    def outr[A,B](p: (A,B)): B = p._2

    def split[A,B1,B2](f1: A => B1, f2: A => B2): A => (B1,B2) =
      a => (f1(a),f2(a))
  }
}

abstract class ChurchProduct[A,B]{
  def apply[C](cont: A => B => C): C
}

object ChurchProduct{

  implicit val churchProduct = new Product[ChurchProduct]{
    def outl[A,B](p: ChurchProduct[A,B]): A = p(a => _ => a)
    def outr[A,B](p: ChurchProduct[A,B]): B = p(_ => b => b)

    def split[A,B1,B2](f1: A => B1, f2: A => B2): A => ChurchProduct[B1,B2] =
      (a: A) => new ChurchProduct[B1,B2]{
        def apply[C](cont: B1 => B2 => C): C =
          cont(f1(a))(f2(a))
      }
  }
}