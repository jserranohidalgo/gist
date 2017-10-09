package org.hablapps.gist

import scalaz._, Scalaz._

import org.scalatest._
class MonadTrans extends FunSpec with Matchers{

  /** Utils */

  type Alg[Σ[_],P[_]] = Σ ~> P

  object Alg{
    trait Iso[TC[_[_]]]{
      type Σ[_]
      def to[F[_]](p: TC[F]): Alg[Σ,F]
      def from[F[_]](p: Alg[Σ,F]): TC[F]
    }

    object Iso{
      type Aux[TC[_[_]],S[_]] = Iso[TC]{ type Σ[T] = S[T] }
    }
  }

  trait Lift[T[_[_],_]]{
    def lift[P[_]: Functor]: P ~> T[P,?]
  }

  object Lift{
    def apply[T[_[_],_]](implicit L: Lift[T]) = L

    implicit def liftWriterT[S: Monoid] = new Lift[WriterT[?[_],S,?]]{
      def lift[P[_]: Functor] = λ[P~>WriterT[P,S,?]]{
        p => WriterT(p map ((mzero[S],_)))
      }
    }
  }

  implicit def MTellState[S,L,F[_]: MonadListen[?[_],L]] = new MonadListen[StateT[F,S,?],L]{
    def point[A](a: => A) = MonadState[StateT[F,S,?],S].point(a)
    def bind[A,B](p: StateT[F,S,A])(f: A => StateT[F,S,B]) =
      MonadState[StateT[F,S,?],S].bind(p)(f)
    def writer[A](w: L, v: A): StateT[F,S,A] =
      StateT{ s => MonadListen[F,L].writer(w,v).map((s,_)) }
    def listen[A](p: StateT[F,S,A]): StateT[F,S,(A,L)] = ???
  }

  /** MonadTell */

  case class Log[Σ[_],T](inst: Σ[T], out: T)

  type InstLog[Σ[_]] = List[Log[Σ,_]]

  object MonadTellTrans{

    def debugF[Σ[_],P[_]: MonadTell[?[_],InstLog[Σ]]]: Alg[Σ,P] => Alg[Σ,P] = io =>
      λ[Alg[Σ,P]]{ inst =>
        io(inst) :++>> ( out => List(Log(inst,out)))
      }

    // def debug[TC[_[_]],P[_]](implicit
    //   iso: Alg.Iso[TC],
    //   M: MonadTell[P,InstLog[iso.Σ]]): TC[P] => TC[P] =

    def debug[TC[_[_]],P[_]](iso: Alg.Iso[TC])(implicit
      tc: TC[P],
      M: MonadTell[P,InstLog[iso.Σ]]): TC[P] =
      iso.to[P](tc) |> debugF[iso.Σ,P](M) |> iso.from[P]

    // def debug[TC[_[_]], S[_], P[_]: MonadTell[?[_],List[Log[S,_]]]](tc: TC[P])(implicit
    //   iso: Alg.Iso[TC]{ type Σ[T] = S[T] }): TC[P] =
    //   iso.to[P](tc) |> debug[S,P] |> iso.from[P]
  }

  /** With monadtrans */

  trait TransS[T[_[_],_[_],_]]{

    def enhance2[Σ[_],P[_]: Functor]: λ[A => (P[A],Σ[A])] ~> T[P,Σ,?]

    def transF2[Σ[_],P[_]: Functor]: Alg[Σ,P] => Alg[Σ,T[P,Σ,?]] = alg =>
      λ[Alg[Σ,T[P,Σ,?]]]{ inst =>
        enhance2[Σ,P].apply(alg(inst),inst)
      }

    def enhance[Σ[_],P[_]: Functor]: λ[A => (T[P,Σ,A],Σ[A])] ~> T[P,Σ,?]

    def transF[Σ[_],P[_]: Functor](implicit L: Lift[T[?[_],Σ,?]]): Alg[Σ,P] => Alg[Σ,T[P,Σ,?]] = alg =>
      λ[Alg[Σ,T[P,Σ,?]]]{ inst =>
        enhance[Σ,P].apply(L.lift[P].apply(alg(inst)),inst)
      }

    def trans[TC[_[_]],Σ[_],P[_]: Functor](implicit
      iso: Alg.Iso.Aux[TC,Σ], tc: TC[P], L: Lift[T[?[_],Σ,?]]): TC[T[P,Σ,?]] =
      iso.from[T[P,Σ,?]](transF[Σ,P].apply(iso.to[P](tc)))
  }

  object DebugTrans extends TransS[λ[(P[_],Σ[_],T) => WriterT[P,InstLog[Σ],T]]]{

    def enhance[Σ[_],P[_]: Functor] =  λ[λ[A => (WriterT[P,InstLog[Σ],A],Σ[A])] ~> WriterT[P,InstLog[Σ],?]]{
      case (w,inst) => w :++>> ( out => List(Log(inst,out)))
    }

    def enhance2[Σ[_],P[_]: Functor] = new (λ[A => (P[A],Σ[A])] ~> WriterT[P,InstLog[Σ],?]){
      def apply[A](v: (P[A],Σ[A])) = v match {
        case (p,inst) => WriterT[P,InstLog[Σ],A](p map ((List(),_))) :++>> {
          out: A => List(Log(inst,out))
        }
      }
    }



    // def transF[Σ[_],P[_]: Functor]: Alg[Σ,P] => Alg[Σ,WriterT[P,InstLog[Σ],?]] = alg =>
    //   λ[Alg[Σ,WriterT[P,InstLog[Σ],?]]]{ inst =>
    //     Lift.liftWriterT[InstLog[Σ]].lift[P].apply(alg(inst)) :++>> ( out => List(Log(inst,out)))
    //   }

    def debug[TC[_[_]],Σ[_],P[_]: Functor](implicit
      iso: Alg.Iso.Aux[TC,Σ],
      tc: TC[P], L: Lift[WriterT[?[_],InstLog[Σ],?]]): TC[WriterT[P,InstLog[iso.Σ],?]] = trans[TC,iso.Σ,P]
      // iso.to[P](_) andThen debugF[iso.Σ,P] andThen iso.from[WriterT[P,InstLog[iso.Σ],?]](_)
      // iso.from[WriterT[P,InstLog[iso.Σ],?]](debugF[iso.Σ,P].apply(iso.to[P](tc)))
  }

  /* IO algebras */

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]
  }

  object IO{ self =>

    sealed abstract class Σ[T]
    case class read() extends Σ[String]
    case class write(msg: String) extends Σ[Unit]

    implicit object iso extends Alg.Iso[IO]{
      type Σ[A] = self.Σ[A]

      def to[P[_]](io: IO[P]): Alg[Σ,P] = λ[Alg[Σ,P]]{
        case read() => io.read()
        case write(msg) => io.write(msg)
      }

      def from[P[_]](io: Alg[Σ,P]): IO[P] = new IO[P]{
        def read() = io(self.read())
        def write(msg: String) = io(self.write(msg))
      }
    }
  }


  /** IO State */

  case class IOState(readList: List[String], writeList: List[String]) {
    def read: IOState = this.copy(readList = readList.tail)
    def write(s: String): IOState = this.copy(writeList = s :: writeList)
  }

  implicit def stateIO[P[_]](implicit M: MonadState[P,IOState]) = new IO[P]{
    def read(): P[String] =
      for {
        s <- M.get
        _ <- M.put(s.read)
      } yield s.readList.head

    def write(msg: String): P[Unit] =
      M.modify(_.write(msg))
  }

  /** IO Programs */

  def echo[P[_]]()(implicit io: IO[P], m: Monad[P]): P[String] =
    io.read() >>! io.write

  /** tests */

  describe("Echo program"){
    it("works for State[IOState,?]"){
      echo[State[IOState,?]]().exec(IOState(List("hi!"),List())) shouldBe
        IOState(List(),List("hi!"))
    }

    it("works for plain StateT[Writer,IOState,?]"){
      echo[StateT[Writer[List[String],?],IOState,?]]().exec(IOState(List("hi!"),List())) shouldBe
        Writer[List[String],IOState](List(),IOState(List(),List("hi!")))
    }

    it("works for plain StateT[Writer,IOState,?] with enhanced interpreter (MonadTell)"){
      import MonadTellTrans._

      type Program[T] = StateT[Writer[InstLog[IO.Σ],?],IOState,T]
      implicit val ml = WriterT.writerTMonadListen[Id,InstLog[IO.Σ]]

      echo[Program]()(
        MonadTellTrans.debug[IO,Program](IO.iso),
        Monad[Program]
      ).exec(IOState(List("hi!"),List())) shouldBe
        Writer[InstLog[IO.Σ],IOState](
          List(Log(IO.read(),"hi!"),Log(IO.write("hi!"),())),
          IOState(List(),List("hi!")))
    }

    it("works for plain StateT[Writer,IOState,?] with enhanced interpreter (WriterT)"){
      import DebugTrans._

      type Program[T] = WriterT[State[IOState,?],InstLog[IO.Σ],T]

      echo[Program]()(
        debug[IO,IO.Σ,State[IOState,?]],
        Monad[Program]
      ).run.eval(IOState(List("hi!"),List())) shouldBe
        (List(Log(IO.read(),"hi!"),Log(IO.write("hi!"),())),"hi!")
    }
  }

  /** */
  // trait MonadTransLift[T[_[_],_],TC[_[_]]]{

  //   def debugF[Σ[_],P[_]: MonadTell[?[_],InstLog[Σ]]]: Alg[Σ,P] => Alg[Σ,P] = io =>

  //   def debug[TC[_[_]],P[_]](iso: Alg.Iso[TC])(implicit
  //     tc: TC[P],
  //     M: MonadTell[P,InstLog[iso.Σ]]): TC[P] =
  //     iso.to[P](tc) |> debugF[iso.Σ,P](M) |> iso.from[P]

  // }

}