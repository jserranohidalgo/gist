package org.hablapps.gist
package companies

import java.util.Date
import Primitive._

case class Company[P[_], S, C, I](
  customers: ListP[Customer, P, C],
  invoices: ListP[Invoice, P, I])

case class Customer[P[_], C](
  cid: IntegerP[P],
  name: StringP[P])

case class Invoice[P[_], I](
  iid: IntegerP[P],
  customer: IntegerP[P],
  due: Field[P, Date],
  amount: IntegerP[P])

object Logic{

  def foo[P[_], S](company: Company[P, S, _, _]): P[List[(Int, Int)]] = ???
}
