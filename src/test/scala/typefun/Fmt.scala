package org.hablapps.gist
package typefun

sealed abstract class Fmt
case class Lit(s: String) extends Fmt
case class Val[A: Read: Show]() extends Fmt
case class Cmp[F1 <: Fmt, F2 <: Fmt](f1: F1, f2: F2) extends Fmt

import org.scalatest._

class FmtSpec extends FunSpec with Matchers{

  val int = Val[Int]

  Lit("day"): Lit
  Cmp(Lit("day"), Lit("s")): Cmp[Lit, Lit]
  Cmp(Lit("day"), int): Cmp[Lit, Val[Int]]
  Cmp(int, Cmp(Lit("day"), Lit("s"))): Cmp[Val[Int], Cmp[Lit, Lit]]
}
