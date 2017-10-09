// import scalaz.~>

// trait MonadState[P[_],A]{
//   def point[B](a: B): P[B]
//   def bind[B,C](p: P[B])(f: B => P[C]): P[C]
//   def put(a: A): P[Unit]
//   def get(): P[A]
// }

// object MonadStateFAlgebra{

//   sealed abstract class Σ[P[_],A,B]
//   case class Get[P[_],A]() extends Σ[P,A,A]
//   case class Put[P[_],A](a: A) extends Σ[P,A,Unit]
//   case class Point[P[_],A,B](b: B) extends Σ[P,A,B]
//   case class Bind[P[_],A,B,C](p: P[B], f: B => P[C]) extends Σ[P,A,C]

//   type MonadState[P[_],A] = Σ[P,A,?] ~> P


//   import scalaz.State
//   val StateInterpretation = λ[Σ[State[(Int,String),?],Int,?]~>State[(Int,String),?]]{
//     case Get() => State.gets[(Int,String),Int](_._1)
//     case Put(s) => State.put[(Int,String)](s)
//     case Point(b) => State.state(b)
//     case Bind(p, f) => p flatMap f
//   }
// }

// sealed abstract class FreeMonadState[A,B]
// case class Get[A]() extends FreeMonadState[A,A]
// case class Put[A](a: A) extends FreeMonadState[A,Unit]
// case class Point[A,B](b: B) extends FreeMonadState[A,B]
// case class Bind[A,B,C](p: FreeMonadState[A,B])(f: B => FreeMonadState[A,C]) extends FreeMonadState[A,C]

// object Examples{

//   val p1: FreeMonadState[Int,Boolean] =
//     Bind(Get())((i: Int) => Point(true))
// }