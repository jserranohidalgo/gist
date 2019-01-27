package org.hablapps.gist.nbe
package systemT

case class Examples[P[_]](implicit ST: SystemT[P]){
  import ST._

  // I = SKK
  def I[T1]: P[T1 => T1] =
    S[T1, Unit => T1, T1](
      K[T1, Unit => T1])(
      K[T1, Unit])
    // (((S[?1, ?2, ?3]: P[(?1 => ?2 => ?3) => (?1 => ?2) => ?1 => ?3])
    //   .apply(K[?4, ?5]: P[?4 => ?5 => ?4]) : P[(?1 => ?2) => ?1 => ?3])
    //   .apply(K[?6, ?7]: P[?6 => ?7 => ?6]) : P[T1 => T1])


  // B = S(KS)K
  def B[T1, T2, T3]: P[(T2 => T3) => (T1 => T2) => (T1 => T3)] =
    S[T2 => T3, T1 => T2 => T3, (T1 => T2) => T1 => T3](
      K[(T1 => T2 => T3) => (T1 => T2) => T1 => T3, T2 => T3](
        S[T1, T2, T3]))(
      K[T2 => T3, T1])
    // ((S[?1, ?2, ?3] : P[(?1 => ?2 => ?3) => (?1 => ?2) => ?1 => ?3])
    //   .apply{
    //     (K[?4, ?5] : P[?4 => ?5 => ?4])
    //       .apply{
    //         (S[?6, ?7, ?8] : P[(?6 => ?7 => ?8) => (?6 => ?7) => ?6 => ?8])
    //       }: P[?5 => ?4]
    //   } : P[(?1 => ?2) => ?1 => ?3])
    //   .apply{
    //     (K[?9, ?10] : P[?9 => ?10 => ?9])
    //   } : P[?1 => ?3]

  // C = S(BBS)(KK)
  def C[T1, T2, T3]: P[(T1 => T2 => T3) => T2 => (T1 => T3)] =
    S[T1 => T2 => T3, T2 => (T1 => T2), T2 => (T1 => T3)](
      B[T1 => T2 => T3, (T1 => T2) => (T1 => T3), (T2 => (T1 => T2)) => (T2 => (T1 => T3))](
        B[T2 , T1 => T2, T1 => T3])(
        S[T1 , T2, T3])
    )(K[T2 => T1 => T2, T1 => T2 => T3](
        K[T2 , T1])
    )
    // ((S[?1 , ?2 , ?3 ]: P[(?1 => ?2 => ?3 ) => (?1 => ?2 ) => ?1 => ?3 ])
    //   .apply(
    //     ((B[?4 , ?5 , ?6 ] : P[(?5 => ?6 ) => (?4 => ?5 ) => (?4 => ?6 )])
    //       .apply(
    //         B[?7 , ?8 , ?9 ] : P[(?8 => ?9 ) => (?7 => ?8 ) => (?7 => ?9 )]
    //       ) : P[(?4 => ?5 ) => (?4 => ?6 )])
    //       .apply(
    //         S[?10 , ?11 , ?12 ] : P[(?10 => ?11 => ?12 ) => (?10 => ?11 ) => ?10 => ?12 ]
    //       ) : P[?4 => ?6 ]
    //   ) : P[(?1 => ?2 ) => ?1 => ?3 ])
    //   .apply(
    //     (K[?13 , ?14 ] : P[?13 => ?14 => ?13 ])
    //       .apply(
    //         (K[?15 , ?16 ] : P[?15 => ?16 => ?15 ])
    //       ) : P[?14 => ?13 ]
    //   ) : P[?1 => ?3 ]

  // ADD = S·REC·(K·(K·SUCC))
  def add: P[Int => Int => Int] =
    ((S: P[(Int => (Int => Int => Int) => (Int => Int)) => (Int => (Int => Int => Int)) => (Int => (Int => Int))])
      .apply{
        (rec: P[Int => (Int => Int => Int) => Int => Int])
      }: P[(Int => (Int => Int => Int)) => (Int => (Int => Int))])
      .apply{
        (K: P[(Int => (Int => Int)) => Int => (Int => (Int => Int))]).apply{
          (K: P[(Int => Int) => Int => (Int => Int)]).apply{
            succ : P[Int => Int]
          } : P[Int => (Int => Int)]
        }: P[Int => (Int => (Int => Int))]
      }: P[(Int => (Int => Int))]



}
