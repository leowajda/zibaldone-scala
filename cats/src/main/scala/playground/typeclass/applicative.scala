package com.zibaldone
package playground.typeclass

package applicative:

  package custom:

    trait Applicative[F[_]] extends apply.custom.Apply[F]:
      def pure[A](a: A): F[A]

  private[applicative] case object example:

    import cats.syntax.applicative.*
    import cats.instances.list.*
    import cats.instances.option.*

    42.pure[List] // List(1)
    42.pure[Option]

    import cats.data.Validated
    type ValidatedErrorOr[T] = Validated[List[String], T]

    // Validated cannot be considered a Monad, but it is a valid Applicative.
    1.pure[ValidatedErrorOr]
