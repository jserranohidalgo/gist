package org.hablapps.gist.nbe
package systemT
package syntax

trait Syntax{

  implicit class AppOp[P[_], T1, T2](f: P[T1 => T2])(implicit ST: SystemT[P]){
    def apply(t1: P[T1]) =
      ST.app[T1, T2](f, t1)
  }
}
