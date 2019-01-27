// package org.hablapps.gist.nbe
// package systemT
// package semantics

// case class Deriv[T](deriv: Derivation[T], termDeriv: TermDer[T])

// sealed abstract class Derivation[T]
// case class Just[T](t: Term[T]) extends Derivation[T]
// case class Step[S, T](f: Derivation[S => T], t: Derivation[S]) extends Derivation[T]

// sealed abstract class TermDer[T]
// case class IntDer(i: Int) extends TermDer[Int]
// case class FunDer[T1, T2](fun: Deriv[T1] => Deriv[T2]) extends TermDer[T1 => T2]

// object Deriv{

//   def derivation[T]: Deriv[T] => Derivation[T] = _.deriv

//   implicit object Sem extends SystemT[Deriv]{

//     def K[T1, T2]: Deriv[T1 => T2 => T1] =
//       Deriv(Just(Term.Sem.K[T1, T2]), FunDer{
//         case (dp, p: TermDer[T1]) =>
//           Deriv(StepFunDer(Term.Sem.app(Term.Sem.K[T1, T2], reify(p)),
//             (q: Deriv[T2]) =>
//               p))

//     def S[T1, T2, T3]: Deriv[(T1 => T2 => T3) => (T1 => T2) => (T1 => T3)] =
//       FunDer(Term.Sem.S[T1, T2, T3],
//         (p: Deriv[T1 => T2 => T3]) =>
//           FunDer(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)),
//             (q: Deriv[T1 => T2]) =>
//               FunDer(Term.Sem.app(Term.Sem.app(Term.Sem.S[T1, T2, T3], reify(p)), reify(q)),
//                 (r: Deriv[T1]) =>
//                   app(app(p,r), app(q,r)))))

//     def app[T1, T2](t1t2: Deriv[T1 => T2], t1: Deriv[T1]): Deriv[T2] =
//       t1t2 match {
//         case FunDer(_, f) => f(t1)
//       }

//     def zero: Deriv[Int] =
//       IntDer(0)

//     def succ: Deriv[Int => Int] =
//       FunDer(Term.Sem.succ, {
//         case IntDer(n) => IntDer(n + 1)
//       })

//     def rec[T]: Deriv[T => (Int => T => T) => (Int => T)] =
//       FunDer(Term.Sem.rec,
//         (t: Deriv[T]) =>
//           FunDer(Term.Sem.app(Term.Sem.rec, reify(t)),
//             (f: Deriv[Int => T => T]) =>
//               FunDer(Term.Sem.app(Term.Sem.app(Term.Sem.rec[T], reify(t)), reify(f)), {
//                 case IntDer(0) => t
//                 case IntDer(n) => app(app(f, IntDer(n-1)), app(app(app(rec[T], t), f), IntDer(n-1)))
//               })))
//   }
// }
