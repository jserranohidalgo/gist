package org.hablapps.gist
package optics
package vanLaarhoven

import scalaz.Applicative

trait Traversal[S,A]{
  def apply[F[_]: Applicative](f: A => F[A]): S => F[S]
}