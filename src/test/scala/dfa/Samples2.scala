package dfa

object Samples2{

  sealed abstract class Q
  case object q0 extends Q
  case object q1 extends Q
  case object q2 extends Q

  sealed abstract class Σ
  case object `0` extends Σ
  case object `1` extends Σ

  val `(0+1)*01(0+1)*`: DFA[Q, Σ] =
    DFA[Q, Σ](
      q0,
      { case (`q0`, `1`) => q0
        case (`q0`, `0`) => q1
        case (`q1`, `0`) => q1
        case (`q1`, `1`) => q2
        case (`q2`, `0`) => q2
        case (`q2`, `1`) => q2},
      Set(q2))
}
