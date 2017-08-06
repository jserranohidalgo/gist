package org.hablapps.gist

import org.scalatest._

class Pairings extends FunSpec with Matchers{
  
  trait Pairing[P[_],Q[_]]{
    def pair[X,Y,R](f: X => Y => R): P[X] => Q[Y] => R
  }

  object Pairing{
    def apply[P[_],Q[_]](implicit P: Pairing[P,Q]) = P

    implicit def ExpProductPairing[A] = new Pairing[A => ?,(A,?)]{
      def pair[X,Y,R](f: X => Y => R): (A => X) => ((A,Y)) => R = 
        e => p => f(e(p._1))(p._2)
        // e => p => Function.uncurried(e andThen f).tupled(p)
    }

    import scalaz.Free, scalaz.Cofree, scalaz.Functor

    implicit def FreeCofree[F[_]: Functor,G[_]](implicit p: Pairing[F,G]) = new Pairing[Free[F,?],Cofree[G,?]]{
      def pair[X,Y,R](f: X => Y => R): Free[F,X] => Cofree[G,Y] => R = 
        _.fold( x => c => f(x)(c.extract),
          ff => c => p.pair(pair(f))(ff)(c.tail))
    }
  }

  import scalaz.Free, scalaz.Cofree, scalaz.State, scalaz.Functor
  trait FreeCofreePairing[F[_],G[_]]{
    val P: Pairing[F,G]
    implicit val F: Functor[F]

    def pair[X,Y,R](f: X => Y => R): Free[F,X] => State[Cofree[G,Y],R] =
      _.fold( 
        x => State.gets(c => f(x)(c.extract)),
        ff => State.get.flatMap{ c => 
          P.pair[Free[F,X],Cofree[G,Y],State[Cofree[G,Y],R]](
            f2 => cf2 => State.put(cf2).flatMap(_ => pair(f)(f2)))(ff)(c.tail) 
        }
      )
  }

  object FreeCofreePairing{
    def apply[F[_],G[_]](implicit P: FreeCofreePairing[F,G]) = P

    implicit def fromPairing[F[_],G[_]](implicit _P: Pairing[F,G], _F: Functor[F]) = new FreeCofreePairing[F,G]{
      val P = _P
      val F = _F
    }

    def StandardPairing[F[_],G[_]](implicit FCP: FreeCofreePairing[F,G]) = new Pairing[Free[F,?],Cofree[G,?]]{
      def pair[X,Y,R](f: X => Y => R): Free[F,X] => Cofree[G,Y] => R = 
        ff => cg => FCP.pair(f)(ff).eval(cg)
    }
  }

  object Collazt{

    sealed abstract class UpDown[T]
    case class Up[T](t: T) extends UpDown[T]
    case class Down[T](t: T) extends UpDown[T]

    object UpDown{
      import scalaz.Functor 
      implicit def upDownFunctor[T] = new Functor[UpDown]{
        def map[A,B](u: UpDown[A])(f: A => B): UpDown[B] = u match {
          case Up(a) => Up(f(a))
          case Down(a) => Down(f(a))
        }
      }
  
      type Cofree[T] = scalaz.Cofree[UpDown,T]
    }

    import scalaz.Functor, scalaz.Cofree
    def unfoldMap[G[_]: Functor,A,B](coalg: A => G[A])(color: A => B): A => Cofree[G,B] =
      a => Cofree.delay(color(a), Functor[G].map(coalg(a))(unfoldMap(coalg)(color)))

    def collazt(n: Integer): UpDown[Integer] = 
      if (n % 2 == 0) Down(n/2)
      else Up(3*n + 1)

    def memoiseCollatz(n: Integer): UpDown.Cofree[Integer] = 
      unfoldMap(collazt)(identity).apply(n)

    case class Two[T](t1: T, t2: T)

    object Two{
      implicit def functorTwo = new Functor[Two]{
        def map[A,B](t: Two[A])(f: A => B) = Two(f(t.t1),f(t.t2))
      }

      implicit val pairingTwoWithUpDown = new Pairing[Two,UpDown]{
        def pair[X,Y,R](f: X => Y => R): Two[X] => UpDown[Y] => R = {
          case Two(t1,t2) => {
            case Up(y) => f(t1)(y)
            case Down(y) => f(t2)(y)
          }
        }
      }

      import scalaz.Free, scalaz.Cofree
      def execute[S,T](program: Free[Two,S => T]): Cofree[UpDown,S] => T = 
        Pairing[Free[Two,?],Cofree[UpDown,?]].pair[S=>T,S,T](f => a => f(a))(program)

      def executeState[S,T](program: Free[Two,S => T]): State[Cofree[UpDown,S],T] = 
        FreeCofreePairing[Two,UpDown].pair[S=>T,S,T](f => a => f(a))(program)

    }
  }

  describe("Collazt machine running"){
    import Collazt._

    sealed abstract class Direction
    case object WentUp extends Direction
    case object WentDown extends Direction

    import scalaz.Free
    def choose: Free[Two,Direction] = 
      Free.roll(Two(Free.pure(WentUp), Free.pure(WentDown)))

    val ex1: Free[Two, Integer => String] = for {
      d1 <- choose
      d2 <- choose
    } yield if (d1 == WentDown && d2 == WentDown) (i => "Went down twice " + i) else (_.toString)
    
    it("should work on program ex1"){
      Two.execute(choose.map(d => (_ : Integer) => d))(memoiseCollatz(12)) shouldBe WentDown

      Two.execute(ex1)(memoiseCollatz(12)) shouldBe "Went down twice 3"

      Two.execute(ex1)(memoiseCollatz(6)) shouldBe "10"
    }

    it("should work with surplus cofree"){
      val machineState1 = memoiseCollatz(12)

      val (machineState2, result1) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState1)
      result1 shouldBe WentDown

      val (machineState3, result2) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState2)
      result2 shouldBe WentDown

      val (machineState4, result3) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState3)
      result3 shouldBe WentUp

      Two.executeState(ex1).eval(machineState4) shouldBe "16"
    }
  }
}