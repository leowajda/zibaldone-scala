package com.zibaldone
package playground.typeclass

package monad:

  import scala.annotation.tailrec

  package custom:

    trait Monad[F[_]] extends flatMap.custom.FlatMap[F] with applicative.custom.Applicative[F]:
      self =>

      def pure[A](value: A): F[A]
      override def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
      override def map[A, B](value: F[A])(f: A => B): F[B] = self.flatMap(value)(a => pure(f(a)))

      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        self.flatMap(fa)(a => self.map(fb)(b => (a, b)))

  private[monad] case object example:

    import cats.Eq
    import cats.Monad
    import cats.instances.list.*
    import cats.syntax.eq.*

    import cats.syntax.applicative.* // .pure
    import cats.syntax.flatMap.*     // .flatMap
    import cats.syntax.functor.*     // .map

    42.pure[List].flatMap(n => n :: n + 1 :: Nil) === List(42, 43)

    final def combinations[F[_]: Monad, A, B](ma: F[A], mb: F[B]): F[(A, B)] =
      for
        a <- ma // .flatMap
        b <- mb // .map
      yield (a, b)

    import cats.instances.either.*
    type ErrorOr[T] = Either[Throwable, T] // also a monad

    final case class Connection(host: String, port: Int)

    sealed trait HttpService[F[_]: Monad]:

      def connection(config: Map[String, String]): F[Connection]
      def request(connection: Connection, payload: String): F[String]

      final def response(config: Map[String, String], payload: String): F[String] =
        for
          conn <- connection(config)
          req  <- request(conn, payload)
        yield req

    object OptHttpService extends HttpService[Option]:

      final override def connection(config: Map[String, String]): Option[Connection] =
        for
          host       <- config.get("host")
          port       <- config.get("port")
          parsedPort <- port.toIntOption
        yield Connection(host, parsedPort)

      final override def request(connection: Connection, payload: String): Option[String] =
        Option.when(payload.length <= 20)(
          s"""request "$payload" is forwarded to ${connection.host}:${connection.port}"""
        )

    object ErrorOrHttpService extends HttpService[ErrorOr]:

      final override def connection(config: Map[String, String]): ErrorOr[Connection] =
        Either.cond(
          test = List("host", "port").map(config.contains).reduce(_ && _) && config("port").toIntOption.isDefined,
          right = Connection(host = config("host"), port = config("port").toInt),
          left = new IllegalStateException
        )

      final override def request(connection: Connection, payload: String): ErrorOr[String] =
        Either.cond(
          test = payload.length <= 20,
          right = s"""request "$payload" is forwarded to ${connection.host}:${connection.port}""",
          left = new IllegalArgumentException
        )

  type Identity[T] = T

  object IdentityMonad extends cats.Monad[Identity]:

    final override def pure[A](x: A): Identity[A] = x

    final override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec final override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => value
