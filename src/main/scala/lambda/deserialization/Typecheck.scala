package org.hablapps.gist
package lambda
package deserialization

import taglessfinal.debruijn._

object Typecheck{

  abstract class Typ{
    type A
    val typ: TQ[A]
  }

  object Typ{
    def apply[_A](t: TQ[_A]) = new Typ{
      type A = _A
      val typ = t
    }
  }

  def read_t2(t: Tree): Either[String, Typ] = t match {
    case Node("TInt", List()) =>
      Right(Typ(tint[TQ]))
    case Node("TArr", List(t1, t2)) =>
      read_t2(t1).right.flatMap{ t1t =>
        read_t2(t2).right.map[Typ]{ t2t =>
          Typ(t1t.typ -> t2t.typ)
        }
      }
    // for{
    //   t1t <- read_t2(t1).right
    //   t2t <- read_t2(t2).right
    // } yield Typ[t1t.A => t2t.A](t1t.typ -> t2t.typ)
    case _ =>
      Left(s"Not a type: $t")
  }

  def read_t(t: Tree): Either[String, TQ[_]] = t match {
    case Node("TInt", List()) =>
      Right(tint[TQ])
    case Node("TArr", List(t1, t2)) => for{
      t1 <- read_t(t1).right
      t2 <- read_t(t2).right
    } yield (t1 -> t2)
    case _ =>
      Left(s"Not a type: $t")
  }

  implicit class EitherOp[A](o: Option[A]){
    def toEither[B](none: B): Either[B, A] =
      o.fold[Either[B, A]](Left(none))(Right(_))
  }

  def apply[P[_, _], Γ, E](tree: Tree, gamma: Γ)(implicit
      L: Lambda[P],
      G: Gamma[Γ, E]): Either[String, DynTerm[P, E]] = tree match {

    case Tree.Int(i) =>
      Right(DynTerm(tint[TQ], L.int(i)))

    case Tree.Add(e1, e2) => for {
      dt1 <- apply(e1, gamma).right
      dt2 <- apply(e2, gamma).right
      _dt1 <- dt1.typ[AsInt].apply(dt1.term).toEither(
        s"First operand of add, not an integer: ${dt1.typ}").right
      _dt2 <- dt2.typ[AsInt].apply(dt2.term).toEither(
        s"Second operand of add, not an integer: ${dt2.typ}").right
    } yield DynTerm(tint[TQ], L.add(_dt1, _dt2))

    case Tree.Var(name) =>
      G.findVar(name, gamma)

    case Tree.Lam(name, typ, body) =>
      read_t2(typ).right.flatMap{ ty1 =>
        apply(body, (Gamma.VarDesc(name, ty1.typ), gamma)).right.map[DynTerm[P, E]]{ db =>
          DynTerm(ty1.typ -> db.typ, L.lam(db.term))
        }
      }
    //   ty1 <- read_t2(typ).right
    //   db <- apply(body, (Gamma.VarDesc(name, ty1.typ), gamma)).right
    // } yield DynTerm(ty1.typ -> db.typ, L.lam(db.term))


    case _ =>
      Left(s"Typecheck error: $tree")
  }
}
