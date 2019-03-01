package org.hablapps.gist
package typefun

trait Read[T]{
  def read(str: String): Option[(String, T)]
}

object Read{

  def apply[A](implicit R: Read[A]): Read[A] = R

  implicit val IntRead = new Read[Int]{
    val IntPrefix = """^(\d+)(.*)""".r

    def read(str: String): Option[(String, Int)] = str match {
      case IntPrefix(i, tail) => Some((tail, Integer.parseInt(i)))
      case _ => None
    }
  }

  implicit def StrRead(prefix: String) = new Read[String]{
    val StrPrefix = (s"""^${prefix}(.*)""").r

    def read(str: String): Option[(String, String)] = {
      str match {
        case StrPrefix(tail) => Some((tail, str))
        case _ => None
      }
    }
  }
}
