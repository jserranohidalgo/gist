package dfa

object Samples{

  sealed abstract class Q
  case object q0 extends Q
  case object q1 extends Q
  case object q2 extends Q

  sealed abstract class Σ
  case object s0 extends Σ
  case object s1 extends Σ

  val `(01)*`: DFA[Q, Σ] =
    DFA[Q, Σ](
      q0,
      { case (`q0`, `s0`) => q1
        case (`q0`, `s1`) => q2
        case (`q1`, `s0`) => q2
        case (`q1`, `s1`) => q0
        case (`q2`, `s0`) => q2
        case (`q2`, `s1`) => q2},
      Set(q0))
}
