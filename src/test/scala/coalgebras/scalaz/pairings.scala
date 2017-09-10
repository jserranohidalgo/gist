package org.hablapps.gist

import scalaz._, Scalaz._

import org.scalatest._

class Pairings extends FunSpec with Matchers{

  /**
   * PRELIMINARIES
   */

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

  // IO- Coalgebras

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

    def run[Alg[_[_]],F[_]: Monad](machine: IOCoChurch[Alg,F]): Church[Alg,?] ~> F =
      λ[Church[Alg,?]~>F]{
        _.fold[StateT[F,machine.S,?]](machine.coalg, Monad[StateT[F,machine.S,?]]).eval(machine.current)
      }

    // implicit def IOCoChurchIOCoalgebra[I[_[_]],F[_],S] = new I[StateT[F,IOCoChurch[I,F],?]]{
    //   ???
    // }
  }

  // (co)Church encodings

  trait Church[Alg[_[_]],T]{
    def fold[P[_]: Alg: Monad]: P[T]
  }

  object Church{

  }



  /** 
   * PIPONI'S SAMPLE
   */

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

  /** 
   * PIPONI'S EXAMPLE WITH IO-COALGEBRAS
   */
  object IOCoalgebrasCollatz{

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
    import IOCoalgebrasCollatz._

    it("should work for ex1"){
      ex1(Monad[State[Int,?]],Collatz).eval(12) shouldBe "Went down twice 3"
      ex1(Monad[State[Int,?]],Collatz).eval(6) shouldBe "10"
    }

  }

  describe("Collatz with church and cochurch"){
    import IOCoalgebrasCollatz._

    it("should work"){
      val ex1Church = new Church[UpDownAlg,String]{
        def fold[P[_]: UpDownAlg: Monad] = ex1[P]
      }
      IOCoChurch.run(IOCoChurch[UpDownAlg,Id,Int](Collatz, 12)).apply(ex1Church) shouldBe "Went down twice 3"
    }
  }

  /** 
   * Dave Laing's EXAMPLE
   */

   // With type classes

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

    it("Works without cofrees"){
      findLimit[State[AdderState,?]].apply((AdderState(3,0))) shouldBe (AdderState(3,0),3)
    }

    it("should work"){
      val findLimitChurch = new Church[Adder,Int]{
        def fold[P[_]: Adder: Monad] = findLimit[P]
      }

      IOCoChurch.run(IOCoChurch[Adder,Id,AdderState](
        AdderState.AdderStateI, AdderState(10,0))).apply(findLimitChurch) shouldBe 10
    }
  }

  // Adder algebra with F-algebras and GADTs

  object AdderGADTAlg{

    // Free monads

    sealed abstract class Free[F[_],T]
    case class Impure[F[_],T](t: F[T]) extends Free[F,T]
    case class Return[F[_],T](t: T) extends Free[F,T]
    case class FlatMap[F[_],T,A,B](p: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

    object Free{
      def foldMap[F[_],P[_]: Monad](variable: F ~> P): Free[F,?] ~> P = 
        λ[Free[F,?] ~> P]{
          case Impure(f) => variable(f)
          case Return(t) => t.pure[P]
          case FlatMap(p,f) => foldMap(variable).apply(p) flatMap (f andThen foldMap(variable).apply)
        }

      def lift[F[_],T](f: F[T]): Free[F,T] = 
        Impure(f)

      implicit def FreeMonadMonad[F[_]] = new Monad[Free[F,?]]{
        def point[A](a: => A): Free[F,A] = 
          Return(a)
        def bind[A,B](p: Free[F,A])(f: A => Free[F,B]): Free[F,B] =
          FlatMap(p,f)
      }
    }

    sealed abstract class AdderF[T]
    case class Add(i: Int) extends AdderF[Boolean]
    case class Clear() extends AdderF[Unit]
    case class Total() extends AdderF[Int]
          
    type AdderProgram[T] = Free[AdderF,T]

    object AdderProgram{
      def add(i: Int): AdderProgram[Boolean] =
        Free.lift(Add(i))
      def clear(): AdderProgram[Unit] =
        Free.lift(Clear())
      def total(): AdderProgram[Int] =
        Free.lift(Total())
    }

    // Programs
    import AdderProgram._

    def findLimit: AdderProgram[Int] = for{
      t <- total()
      _ <- clear()
      limit <- findLimitAux
      _ <- clear()
      _ <- add(t)
    } yield limit

    def findLimitAux: AdderProgram[Int] = for {
      overflow <- add(1)
      limit <- if (overflow) total() else findLimitAux
    } yield limit

    // Interpreters

    case class AdderState(limit: Int, current: Int)

    object AdderState{

      implicit val stateVar: AdderF ~> State[AdderState,?] = 
        λ[AdderF ~> State[AdderState,?]]{
          case Add(i) => 
            State[AdderState,Boolean]{
              case s@AdderState(total,current) =>
                if (current + i > total) (s,true)
                else (AdderState(total, current+i),false) 
            }    
          case Clear() => 
            State.modify[AdderState](_.copy(current=0))
          case Total() => 
            State.gets[AdderState,Int](_.current)
        }

      implicit def run[X]: AdderProgram[X] => State[AdderState,X] =
        Free.foldMap(stateVar).apply[X]
    }

  }

  describe("GADT-based adder"){
    import AdderGADTAlg._

    it("Works"){
      AdderState.run[Int](findLimit).apply((AdderState(3,0))) shouldBe (AdderState(3,0),3)
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

      def AlgFreeAlg[F[_],X]: F[FreeAlg[F,X]] => FreeAlg[F,X] = 
        Join(_)

      def lift[F[_]: Functor,X](fx: F[X]): FreeAlg[F,X] = 
        Join(fx map Pure.apply)

      def foldMap[F[_]: Functor,X,Y](alg: F[Y] => Y)(f: X=>Y): FreeAlg[F,X]=>Y = {
        case Pure(x) => f(x)
        case Join(cont) => alg(cont map foldMap(alg)(f))
      }

      // in terms of Forall
      def foldMap[F[_]: Functor,M[_]: Applicative](alg: Forall[λ[X => Alg[F,M[X]]]]): FreeAlg[F,?] ~> M = 
        new (FreeAlg[F,?] ~> M){
          def apply[X](p: FreeAlg[F,X]): M[X] = 
            foldMap[F,X,M[X]](alg[X])(_.pure[M]).apply(p)
        }

      // in terms of natural transformations
      def foldMap[F[_]: Functor,M[_]: Applicative](alg: λ[X => F[M[X]]]~>M): FreeAlg[F,?] ~> M = 
        new (FreeAlg[F,?] ~> M){
          def apply[X](p: FreeAlg[F,X]): M[X] = 
            foldMap[F,X,M[X]](alg[X])(_.pure[M]).apply(p)
        }

      def interpreter[F[_]: Functor, M[_]: Monad](alg: F ~> M): FreeAlg[F,?] ~> M = 
        λ[FreeAlg[F,?] ~> M]{
          case Pure(x) => 
            Monad[M].pure(x)
          case Join(inst) =>  
            alg(inst) flatMap interpreter(alg).apply
        }
    }



    // Adder Algebra

    sealed abstract class AdderF[T]
    case class add[T](i: Int, k: Boolean => T) extends AdderF[T]
    case class clear[T](k: T) extends AdderF[T]
    case class total[T](k: Int => T) extends AdderF[T]

    object AdderF{
      type Alg[T] = self.Alg[AdderF,T]
      
      implicit val FF = new Functor[AdderF]{
        def map[A,B](p: AdderF[A])(f: A => B) = p match {
          case add(i, k) => self.add(i, k andThen f)
          case clear(k) => self.clear(f(k))
          case total(k) => self.total(k andThen f)
        }
      }
    }

    type AdderProgram[T] = FreeAlg[AdderF,T]

    object AdderProgram{
      def add(i: Int): AdderProgram[Boolean] =
        FreeAlg.lift(self.add(i,identity))
      def clear(): AdderProgram[Unit] =
        FreeAlg.lift(self.clear(()))
      def total(): AdderProgram[Int] =
        FreeAlg.lift(self.total(identity))
    }

    // Adder Interpreter

    case class AdderState(limit: Int, current: Int)

    object AdderState{

      implicit def interpreter[X](s: AdderState): AdderProgram[X] => (AdderState,X) = {
        case Pure(y) => (s,y)
        case Join(add(i, k)) =>
          if (s.current + i > s.limit) interpreter(s)(k(true))
          else interpreter(AdderState(s.limit, s.current+i))(k(false))
        case Join(clear(k)) => 
          interpreter(s.copy(current=0))(k)
        case Join(total(k)) => 
          interpreter(s)(k(s.current))
      }

      implicit def interpreter[X]: AdderProgram[X] => State[AdderState,X] = {
        case Pure(y) => 
          State.state(y)
        case Join(add(i,k)) => 
          State[AdderState,Boolean]{
            case s@AdderState(total,current) =>
              if (current + i > total) (s,true)
              else (AdderState(total, current+i),false) } >>= 
          (k andThen interpreter)
        case Join(clear(k)) => 
          State.modify[AdderState](_.copy(current=0)) >> 
          interpreter[X].apply(k)
        case Join(total(k)) => 
          State.gets[AdderState,Int](_.current) >>= 
          (k andThen interpreter)
      }
      
      implicit val stateAlg = new Forall[λ[X=>AdderF.Alg[State[AdderState,X]]]]{
        def apply[X] = {
          case add(i,k) => 
            State[AdderState,Boolean]{
              case s@AdderState(total,current) =>
                if (current + i > total) (s,true)
                else (AdderState(total, current+i),false) } >>= 
            k
          case clear(k) => 
            State.modify[AdderState](_.copy(current=0)) >> 
            k
          case total(k) => 
            State.gets[AdderState,Int](_.current) >>= 
            k
        }
      }

      implicit val stateAlg_1 = new Forall[λ[X=>AdderF.Alg[State[AdderState,X]]]]{
        def apply[X]: AdderF.Alg[State[AdderState,X]] =
          stateAlg2(_).join
      }
      
      implicit def interpreter2[X]: AdderProgram[X] => State[AdderState,X] =
        // FreeAlg.foldMap[AdderF,X,State[AdderState,X]](alg[X])(State.state)
        FreeAlg.foldMap(stateAlg_1).apply[X]


      implicit val stateAlg2: AdderF ~> State[AdderState,?] = 
        new (AdderF ~> State[AdderState,?]){
          def apply[X](adderF: AdderF[X]) = adderF match {
            case add(i,k) => 
              State[AdderState,Boolean]{
                case s@AdderState(total,current) =>
                  if (current + i > total) (s,true)
                  else (AdderState(total, current+i),false) } >>=
              (k andThen State.state)    
            case clear(k) => 
              State.modify[AdderState](_.copy(current=0)) >> 
              State.state(k)
            case total(k) => 
              State.gets[AdderState,Int](_.current) >>= 
              (k andThen State.state)
          }
        }

      implicit val stateAlg2_1: AdderF ~> State[AdderState,?] = 
        λ[AdderF ~> State[AdderState,?]]{
          f => stateAlg.apply(f.map(State.state))
        }

      implicit def interpreter3[X]: AdderProgram[X] => State[AdderState,X] =
        FreeAlg.interpreter(stateAlg2_1).apply[X]

    }

    // Programs
    import AdderProgram._

    def findLimit: AdderProgram[Int] = for{
      t <- AdderProgram.total()
      _ <- AdderProgram.clear()
      limit <- findLimitAux
      _ <- AdderProgram.clear()
      _ <- AdderProgram.add(t)
    } yield limit

    def findLimitAux: AdderProgram[Int] = for {
      overflow <- AdderProgram.add(1)
      limit <- if (overflow) AdderProgram.total() else findLimitAux
    } yield limit

  }

  describe("ADT-based adder"){
    import AdderADTAlg._

    it("Works"){
      AdderState.interpreter(AdderState(3,0))(findLimit) shouldBe (AdderState(3,0),3)
      AdderState.interpreter[Int](findLimit).apply((AdderState(3,0))) shouldBe (AdderState(3,0),3)
      AdderState.interpreter2[Int](findLimit).apply((AdderState(3,0))) shouldBe (AdderState(3,0),3)
      AdderState.interpreter3[Int](findLimit).apply((AdderState(3,0))) shouldBe (AdderState(3,0),3)
    }
  }

}