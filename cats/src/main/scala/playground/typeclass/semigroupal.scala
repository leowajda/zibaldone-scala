package com.zibaldone
package playground.typeclass

package semigroupal:

  package custom:

    // monad is a semigroupal where the product is equal to a cartesian product
    trait Semigroupal[F[_]]:
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  private[semigroupal] case object example:

    import cats.Semigroupal
    import cats.instances.option.*

    Semigroupal[Option].product[Int, Int](Some(42), None) // None

    import cats.instances.future.*
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    Semigroupal[Future].product(Future("meaningOfLife"), Future(42)) // Future(("meaningOfLife", 42))

    import cats.instances.list.*
    Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, "a"), (1, "b"), (2, "a") (2, "b"))

    import cats.Monad
    import cats.syntax.flatMap.* // .flatMap
    import cats.syntax.functor.* // .map

    final def monadicProduct[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      for
        a <- fa
        b <- fb
      yield (a, b)

    import cats.data.Validated

    type ValidatedErrorOr[T] = Validated[List[String], T]
    type EitherErrorOr[T]    = Either[List[String], T]

    // ValidatedErrorOr[(String, String)]
    // Semigroupal[_] doesn't necessarily need to be implemented in terms of .flatMap,
    // Validated provides a custom version of product that propagates errors in the correct manner.
    Semigroupal[ValidatedErrorOr].product(
      fa = Validated.invalid[List[String], String]("a" :: "b" :: Nil),
      fb = Validated.invalid[List[String], String]("c" :: Nil)
    )

    // EitherErrorOr[(String, String)]
    // because Semigroupal[Either[_, _]] is implemented in terms of .flatMap
    // the error handling is short-circuiting so information is being lost.
    Semigroupal[EitherErrorOr].product(
      fa = Left[List[String], String]("a" :: "b" :: Nil),
      fb = Left[List[String], String]("c" :: Nil)
    )
