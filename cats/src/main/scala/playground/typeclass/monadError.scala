package com.zibaldone
package playground.typeclass

package monadError:

  package custom:

    trait ApplicativeError[F[_], E] extends applicative.custom.Applicative[F]:
      self =>

      def raiseError[A](err: E): F[A]

      def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

      def handleError[A](fa: F[A])(f: E => A): F[A] =
        self.handleErrorWith(fa)(e => self.pure(f(e)))

    trait MonadError[F[_], E] extends ApplicativeError[F, E] with monad.custom.Monad[F]:

      def ensure[A](fa: F[A])(err: => E)(predicate: E => Boolean): F[A]

  private[monadError] case object example:

    import cats.MonadError
    import cats.instances.either.*

    type ErrorOr[T] = Either[String, T]

    val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
    val failure: ErrorOr[Int]                   = monadError.raiseError[Int]("something wrong")
    val pure: ErrorOr[Int]                      = monadError.pure[Int](42)

    monadError.handleError(failure)(_ => 42)
    monadError.handleErrorWith(failure)(_ => pure)
    monadError.ensure(pure)("not meaning of life")(_ == 42)

    import scala.util.Try
    import cats.instances.try_.* // MonadThrow[Try]

    MonadError[Try, Throwable].raiseError[Nothing](new RuntimeException) // Failure(new RuntimeException)

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import cats.instances.future.* // MonadThrow[Future]

    MonadError[Future, Throwable].pure[Int](42)

    import cats.data.Validated
    import cats.ApplicativeError
    import cats.instances.vector.*

    type ValidateOr[T] = Validated[Vector[Throwable], T]
    ApplicativeError[ValidateOr, Vector[Throwable]]

    import cats.syntax.applicative.*      // .pure
    import cats.syntax.applicativeError.* // .raiseError .handleError .handleErrorWith
    import cats.syntax.monadError.*       // .ensure

    42.pure[ValidateOr]
    Vector(new RuntimeException).raiseError[ValidateOr, Int]
    pure.ensure("not meaning of life")(_ == 42)
