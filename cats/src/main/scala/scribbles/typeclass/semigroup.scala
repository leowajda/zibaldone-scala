package com.zibaldone
package scribbles.typeclass

import cats.Semigroup
import cats.syntax.semigroup.*

// |+| is always associative
def reduce[A: Semigroup](coll: Iterable[A]): A = coll.reduce(_ |+| _)

final case class Expense(id: Long, amount: Double)

object Expense:

  given semigroup(using Semigroup[Double]): Semigroup[Expense] =
    Semigroup.instance: (a, b) =>
      Expense(a.id max b.id, a.amount |+| b.amount)
