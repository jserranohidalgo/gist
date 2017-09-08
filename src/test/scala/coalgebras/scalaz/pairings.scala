package org.hablapps.gist

import scalaz._, Scalaz._

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

    implicit def FreeCofree[F[_]: Functor,G[_]](implicit p: Pairing[F,G]) = new Pairing[Free[F,?],Cofree[G,?]]{
      def pair[X,Y,R](f: X => Y => R): Free[F,X] => Cofree[G,Y] => R =
        _.fold( x => c => f(x)(c.extract),
          ff => c => p.pair(pair(f))(ff)(c.tail))
    }
  }

  trait FreeCofreePairing[F[_],G[_]]{
    val P: Pairing[F,G]
    implicit val F: Functor[F]

    def pair[X,Y,R](f: X => Y => R): Free[F,X] => State[Cofree[G,Y],R] =
      _.fold(
        x => State.gets(c => f(x)(c.extract)),
        ff => State.get[Cofree[G,Y]] >>= { c =>
          P.pair[Free[F,X],Cofree[G,Y],State[Cofree[G,Y],R]](
            f2 => cf2 => State.put(cf2) >> pair(f)(f2))(ff)(c.tail)
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
      implicit def upDownFunctor[T] = new Functor[UpDown]{
        def map[A,B](u: UpDown[A])(f: A => B): UpDown[B] = u match {
          case Up(a) => Up(f(a))
          case Down(a) => Down(f(a))
        }
      }

      type Cofree[T] = scalaz.Cofree[UpDown,T]
    }

    def unfoldMap[G[_]: Functor,A,B](coalg: A => G[A])(color: A => B): A => Cofree[G,B] =
      a => Cofree.delay(color(a), Functor[G].map(coalg(a))(unfoldMap(coalg)(color)))

    def unfoldAdHoc[B,G[_]: Functor](b: B, values: G[B]*): Cofree[λ[t => Option[G[t]]],B] =
      Cofree[λ[t => Option[G[t]]],B](b,
        values.seq.foldRight[Option[G[Cofree[λ[t => Option[G[t]]],B]]]](Option.empty){
          case (g,cf) => Some(g map { b1 => Cofree[λ[t => Option[G[t]]],B](b1,cf)})
        })

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

    // it("should work with ad-hoc history"){
    //   import Collazt._

    //   val machineState1 = unfoldAdHoc[Integer,UpDown](12,Down(6),Down(3),Up(10))

    //   val (machineState2, result1) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState1)
    //   result1 shouldBe WentDown

    //   val (machineState3, result2) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState2)
    //   result2 shouldBe WentDown

    //   // val (machineState4, result3) = Two.executeState(choose.map(d => (_ : Integer) => d))(machineState3)
    //   // result3 shouldBe WentUp

    //   // Two.executeState(ex1).eval(machineState4) shouldBe "16"
    // }
  }

  object IOCoalgebras{

    // UpDown coalgebra

    sealed abstract class UpDown
    case object Up extends UpDown
    case object Down extends UpDown

    trait UpDownAlg[P[_]]{
      def next(): P[UpDown]
      def current(): P[Int]
    }

    type UpDownMachine[S] = UpDownAlg[State[S,?]]

    object Collatz extends UpDownMachine[Int]{
      def next(): State[Int,UpDown] = State{ n =>
        if (n % 2 ==0) (n/2,Down)
        else (3*n+1,Up)
      }
      def current(): State[Int,Int] = State.get
    }

    def ex1[P[_]: Monad](implicit UD: UpDownAlg[P]): P[String] = for{
      d1 <- UD.next()
      d2 <- UD.next()
      current <- UD.current()
    } yield if (d1==Down & d2==Down) s"Went down twice $current" else s"$current"

  }

  describe("IOCoalgebra for Collatz"){
    import IOCoalgebras._

    it("should work for ex1"){
      ex1(Monad[State[Int,?]],Collatz).eval(12) shouldBe "Went down twice 3"
      ex1(Monad[State[Int,?]],Collatz).eval(6) shouldBe "10"
    }

  }


  type IOCoalgebra[Alg[_[_]],F[_],S] = Alg[StateT[F,S,?]]

  trait IOCoChurch[Alg[_[_]],F[_]]{
    type S
    val coalg: IOCoalgebra[Alg,F,S]
    val current: S
  }

  object IOCoChurch{

    def apply[Alg[_[_]],F[_],_S](_coalg: IOCoalgebra[Alg,F,_S], _current: _S) = new IOCoChurch[Alg,F]{
      type S = _S
      val coalg = _coalg
      val current = _current
    }

    def unfold[Alg[_[_]],F[_],S](coalg: IOCoalgebra[Alg,F,S]): S => IOCoChurch[Alg,F] =
      apply(coalg,_)

    // implicit def IOCoChurchIOCoalgebra[I[_[_]],F[_],S] = new I[StateT[F,IOCoChurch[I,F],?]]{
    //   ???
    // }
  }

  trait Church[Alg[_[_]],T]{
    def fold[P[_]: Alg: Monad]: P[T]
  }

  object Church{

    def run[Alg[_[_]],F[_]: Monad](machine: IOCoChurch[Alg,F]): Church[Alg,?] ~> F =
      λ[Church[Alg,?]~>F]{
        _.fold[StateT[F,machine.S,?]](machine.coalg, Monad[StateT[F,machine.S,?]]).eval(machine.current)
      }
  }

  describe("Collatz with church and cochurch"){
    import IOCoalgebras._

    it("should work"){
      val ex1Church = new Church[UpDownAlg,String]{
        def fold[P[_]: UpDownAlg: Monad] = ex1[P]
      }
      Church.run(IOCoChurch[UpDownAlg,Id,Int](Collatz, 12)).apply(ex1Church) shouldBe "Went down twice 3"
    }
  }


  object PairingDLaing{

    // Adder algebra with type classes

    trait Adder[P[_]]{
      def add(i: Int): P[Boolean]
      def clear(): P[Unit]
      def total(): P[Int]
    }

    // Interpreter

    case class AdderState(limit: Int, current: Int)

    object AdderState{
      implicit object AdderStateI extends Adder[State[AdderState,?]]{
        def add(i: Int): State[AdderState,Boolean] = State{
          case s@AdderState(total,current) =>
            if (current + i > total) (s,true)
            else (AdderState(total, current+i),false)
        }
        def clear() = State.modify(_.copy(current=0))
        def total() = State.gets(_.current)
      }
    }

    // Programs

    def findLimit[P[_]: Monad](implicit A: Adder[P]): P[Int] = for{
      total <- A.total()
      _ <- A.clear()
      limit <- findLimitAux[P]
      _ <- A.clear
      _ <- A.add(total)
    } yield limit

    def findLimitAux[P[_]: Monad](implicit A: Adder[P]): P[Int] = for {
      overflow <- A.add(1)
      limit <- if (overflow) A.total() else findLimitAux[P]
    } yield limit

  }

  describe("Adder"){
    import PairingDLaing._

    it("should work"){
      val findLimitChurch = new Church[Adder,Int]{
        def fold[P[_]: Adder: Monad] = findLimit[P]
      }

      Church.run(IOCoChurch[Adder,Id,AdderState](
        AdderState.AdderStateI, AdderState(10,0))).apply(findLimitChurch) shouldBe 10
    }
  }

      // Adder algebra with F-algebras and GADTs

  object AdderGADTAlg{

    sealed abstract class AdderF[T]
    case class Add(i: Int) extends AdderF[Boolean]
    case class Clear() extends AdderF[Unit]
    case class Total() extends AdderF[Int]

    import scalaz.~>
    type AdderAlg[P[_]] = AdderF ~> P

    sealed abstract class Free[F[_],T]
    case class Impure[F[_],T](t: F[T]) extends Free[F,T]
    case class Return[F[_],T](t: T) extends Free[F,T]
    case class FlatMap[F[_],T,A,B](p: Free[F,A])(f: A => Free[F,B]) extends Free[F,B]

    object Free{
      type AdderProgram[T] = Free[AdderF,T]

      val AdderAlgFree = new AdderAlg[Free[AdderF,?]]{
        def apply[T](f: AdderF[T]): Free[AdderF,T] = Impure(f)
      }

      def foldMap[F[_],P[_]: Monad](alg: F ~> P): Free[F,?] ~> P =
        ???
    }
  }

  // Adder algebra with F-algebras and GADTs

  object AdderADTAlg{ self =>

    // Algebras & Free algebras

    type Alg[F[_],T] = F[T] => T

    sealed abstract class FreeAlg[F[_],_]
    case class Pure[F[_],X](x: X) extends FreeAlg[F,X]
    case class Join[F[_],X](cont: F[FreeAlg[F,X]]) extends FreeAlg[F,X]

    object FreeAlg{
      implicit def FreeAlgMonad[F[_]: Functor] = new Monad[FreeAlg[F,?]]{
        def point[A](a: => A) = Pure(a)
        def bind[A,B](p: FreeAlg[F,A])(f: A => FreeAlg[F,B]): FreeAlg[F,B] =
          p match {
            case Pure(x) => f(x)
            case Join(cont) => Join(cont map (bind(_)(f)))
          }
      }
    }

    def foldMap[F[_]: Functor,X,Y](alg: F[Y] => Y)(f: X=>Y): FreeAlg[F,X]=>Y = {
      case Pure(x) => f(x)
      case Join(cont) => alg(cont map foldMap(alg)(f))
    }

    // Adder Algebra

    sealed abstract class AdderF[T]
    case class add[T](i: Int, k: Boolean => T) extends AdderF[T]
    case class clear[T](k: T) extends AdderF[T]
    case class total[T](k: Int => T) extends AdderF[T]

    object AdderF{
      def add(i: Int): AdderProgram[Boolean] =
        Join(self.add(i,Pure.apply))
      def clear(): AdderProgram[Unit] =
        Join(self.clear(Pure(())))
      def total(): AdderProgram[Int] =
        Join(self.total(Pure.apply))

      implicit val FF = new Functor[AdderF]{
        def map[A,B](p: AdderF[A])(f: A => B) = p match {
          case add(i, k) => self.add(i, k andThen f)
          case clear(k) => self.clear(f(k))
          case total(k) => self.total(k andThen f)
        }
      }
    }

    type AdderAlg[T] = Alg[AdderF,T]

    type AdderProgram[T] = FreeAlg[AdderF,T]

    // Adder Interpreter

    case class AdderState(limit: Int, current: Int)

    // object AdderState{
    //   implicit def AdderStateI[Y](s: AdderState): AdderProgram[Y] => Y = {
    //     case Pure(y) => y
    //     case Join(add(i, k)) =>
    //       case s@AdderState(total,current) =>
    //         if (current + i > total) (s,true)
    //         else (AdderState(total, current+i),false)
    //     }
    //     case Join(clear(k)) => State.modify(_.copy(current=0))
    //     case Join(total(k)) => State.gets(_.current)
    //   }
    // }

    // Programs
    import AdderF._

    def findLimit: AdderProgram[Int] = for{
      t <- AdderF.total()
      _ <- AdderF.clear()
      limit <- findLimitAux
      _ <- AdderF.clear()
      _ <- AdderF.add(t)
    } yield limit

    def findLimitAux: AdderProgram[Int] = for {
      overflow <- AdderF.add(1)
      limit <- if (overflow) AdderF.total() else findLimitAux
    } yield limit

  }


}