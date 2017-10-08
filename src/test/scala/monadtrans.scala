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

  object MonadTellTrans{
    
    case class Log[Σ[_],T](inst: Σ[T], out: T)

    type InstLog[Σ[_]] = List[Log[Σ,_]]

    def tellF[Σ[_],P[_]: MonadTell[?[_],InstLog[Σ]]]: Alg[Σ,P] => Alg[Σ,P] = io =>
      λ[Alg[Σ,P]]{ inst => 
        io(inst) :++>> ( out => List(Log(inst,out))) 
      }

    def tell[TC[_[_]],P[_]](iso: Alg.Iso[TC])(implicit 
      tc: TC[P],
      M: MonadTell[P,InstLog[iso.Σ]]): TC[P] =
      iso.to[P](tc) |> tellF[iso.Σ,P](M) |> iso.from[P]

    // def tell[TC[_[_]], S[_], P[_]: MonadTell[?[_],List[Log[S,_]]]](tc: TC[P])(implicit
    //   iso: Alg.Iso[TC]{ type Σ[T] = S[T] }): TC[P] = 
    //   iso.to[P](tc) |> tell[S,P] |> iso.from[P]
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

    object iso extends Alg.Iso[IO]{
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

    /* lift to monad tell */
    import MonadTellTrans.Log

    def tell[P[_]: MonadTell[?[_],List[Log[Σ,_]]]](io: IO[P]): IO[P] = 
      iso.to[P](io) |> tell2[P] |> iso.from[P]
    // new IO[P]{
    //   def read(): P[String] = 
    //     io.read() :++>> (msg => List(Log(self.read(),msg)))
    //   def write(msg: String): P[Unit] = 
    //     io.write(msg) :++> List(Log(self.write(msg),()))
    // }

    def tell2[P[_]: MonadTell[?[_],List[Log[Σ,_]]]]: Alg[Σ,P] => Alg[Σ,P] = io => 
      λ[Alg[Σ,P]]{ inst => io(inst) :++>> ( out => List(Log(inst,out)))
        // case read() => 
        //   io(self.read()) :++>> (msg => List(Log(self.read(),msg)))
        // case write(msg) =>
        //   io(self.write(msg)) :++> List(Log(self.write(msg),()))
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

    it("works for plain StateT[Writer,IOState,?] with enhanced interpreter"){
      import MonadTellTrans._

      type Program[T] = StateT[Writer[InstLog[IO.Σ],?],IOState,T]
      implicit val ml = WriterT.writerTMonadListen[Id,InstLog[IO.Σ]]

      echo[Program]()(
        MonadTellTrans.tell[IO,Program](IO.iso),
        Monad[Program]
      ).exec(IOState(List("hi!"),List())) shouldBe
        Writer[InstLog[IO.Σ],IOState](
          List(Log(IO.read(),"hi!"),Log(IO.write("hi!"),())),
          IOState(List(),List("hi!")))
    }
  }



}