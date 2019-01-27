package org.hablapps.gist.nbe
package systemT
package semantics

object ShowSem extends SystemT[Show]{

  def K[T1, T2] = "K"

  def S[T1, T2, T3] = "S"

  def app[T1, T2](f: String, t1: String) = s"($f$t1)"

  def zero = "0"

  def succ = "S"

  def rec[T] = "rec"
}
