package org.hablapps.gist

import scalaz._, Scalaz._

object Stateless{

  trait EitherState[P[_], L[_], R[_]]{
    def fold[A](left: L[A], right: R[A]): P[A]
  }

  object EitherState{
    implicit def forState[A,B] = 
      new EitherState[State[Either[A,B],?],
        State[A,?], State[B,?]]{
          def fold[T](left: State[A,T], right: State[B,T]) = 
            State{ _.fold(
              left(_).swap.map(Left(_)).swap, 
              right(_).swap.map(Right(_)).swap 
            )}
        }
  }

}


object Speech{
  import Stateless._

  trait Agent[Ag]{
    type P[_]
    val name: P[String]
  }

  object Agent{
    type For[P1[_],Ag] = Agent[Ag]{ type P[t] = P1[t] }

    implicit def forEither[P1[_], L[_], R[_], A1, A2](implicit 
      A1: Agent.For[L,A1], 
      A2: Agent.For[R,A2],
      MS: EitherState[P1,L,R]) = new Agent[Either[A1,A2]]{
        type P[t] = P1[t]
        val name: P[String] = MS.fold(A1.name,A2.name)
    }
  }

  trait Context[I]{
    type P[_]
    type Ag; implicit val Agent: Agent[Ag]

    val members: P[List[Ag]]
    def play(a: Ag): P[Unit]
    def abandon(a: Ag): P[Unit]
  }
}

object Movies{
  import Speech._

  trait Actor[A]{ self => 
    type P[_]

    implicit val Agent: Agent[A]{
      type P[t] = self.P[t]
    }
  }

  trait Director[D]{ self => 
    type P[_]

    implicit val Agent: Agent[D]{
      type P[t] = self.P[t]
    }
  }

  trait Film[F]{ self =>
    type P[_]
    
    type Ac; implicit val Actor: Actor[Ac]
    type Di; implicit val Director: Director[Di]

    val Context: Context[F]{
      type P[t] = self.P[t]
      type Ag = Either[Ac,Di]
    }

    def actors(implicit F: Functor[P]): P[List[Ac]] = 
      Context.members.map{
        _.foldLeft[List[Ac]](List()){
          case (acc, m) =>
            m.fold(_ +: acc, _ => acc)
        }
      }
  }
}

object Instance{
  import Stateless._, Speech._, Movies._

  case class SActor(name: String)
  
  object SActor{

    implicit val SActorAgent =  new Agent[SActor]{
      type P[t] = State[SActor,t]
      val name: State[SActor,String] = 
        State.gets(_.name)
    }

    implicit val SActorActor = new Actor[SActor]{ self => 
      type P[t] = State[SActor,t]
      val Agent = SActorAgent
    }
  }

  case class SDirector(name: String)
  
  object SDirector{

    implicit val SDirectorAgent = new Agent[SDirector]{
      type P[t] = State[SDirector,t]
      val name: State[SDirector,String] = 
        State.gets(_.name)
    }

    implicit val SDirectorDirector = new Director[SDirector]{ self => 
      type P[t] = State[SDirector,t]
      val Agent = SDirectorAgent
    }
  }

  case class SFilm(director: SDirector, actors: List[SActor])

  object SFilm{

    implicit val SFilmContext = new Context[SFilm]{
      type P[t] = State[SFilm,t]
      type Ag = Either[SActor,SDirector]

      val Agent: Agent[Ag] = Speech.Agent.forEither[
        State[Either[SActor,SDirector],?],
        SActor.SActorAgent.P,
        SDirector.SDirectorAgent.P,
        SActor,
        SDirector]
      val members: P[List[Ag]] = ???
      def play(a: Ag): P[Unit] = ???
      def abandon(a: Ag): P[Unit] = ???
    }

    implicit val SFilmFilm = new Film[SFilm]{ self => 
      type P[t] = State[SFilm,t]
      val Context = SFilmContext
      type Ac = SActor; val Actor = SActor.SActorActor
      type Di = SDirector; val Director = SDirector.SDirectorDirector
    }
  }

}
