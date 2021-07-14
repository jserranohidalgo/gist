package dfa

case class DFA[Q, Σ](initial: Q, f: (Q, Σ) => Q, finals: Set[Q]){
  val fext: (Q, List[Σ]) => Q =
    (q, word) => word.foldLeft(q)(f)
}

object DFA{
  def run[Q, Σ](dfa: DFA[Q, Σ])(word: List[Σ]): Q =
    dfa.fext(dfa.initial, word)

  def accept[Q, Σ](dfa: DFA[Q, Σ])(word: List[Σ]): Boolean =
    dfa.finals(run(dfa)(word))
}
