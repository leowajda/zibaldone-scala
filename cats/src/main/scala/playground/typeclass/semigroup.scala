package com.zibaldone
package playground.typeclass

package semigroup:

  package custom:

    // semigroups combine elements of the same type
    trait Semigroup[A]:
      def combine(a: A, b: A): A

  private[semigroup] case object example:

    import cats.Semigroup
    import cats.syntax.semigroup.*

    // |+| is always associative
    final def reduce[A: Semigroup](coll: Iterable[A]): A = coll.reduce(_ |+| _)

    final case class Expense(id: Long, amount: Double)

    object Expense:

      given semigroup(using Semigroup[Double]): Semigroup[Expense] =
        Semigroup.instance: (a, b) =>
          Expense(a.id max b.id, a.amount |+| b.amount)
