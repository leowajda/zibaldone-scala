package com.zibaldone
package playground.advanced

package contravariant:

  package custom:

    trait Contravariant[F[_]] extends invariant.custom.Invariant[F]:
      self =>

      def contramap[A, B](fa: F[A])(back: B => A): F[B]

      override def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B] =
        self.contramap(fa)(back)

  private[contravariant] case object example:

    trait Format[A]:
      self =>

      def format(value: A): String

      // contravariant type class
      // for the implicit type-class derivation the evaluation order for summoning F[F[A]]
      // is in reverse (stacked f(value) calls)
      final def contramap[B](f: B => A): Format[B] =
        (value: B) => self.format(f(value))

    // given an implicit instance of T, how to derive F[T]?
    given Format[Int] = _.toString

    import cats.Contravariant
    import cats.Show
    import cats.instances.int.*

    val showInt: Show[Int]            = Show[Int]
    val optShowInt: Show[Option[Int]] = Contravariant[Show].contramap(showInt)(_.getOrElse(-1))

    import cats.syntax.contravariant.*
    showInt.contramap[Option[Int]](_.getOrElse(-1))
