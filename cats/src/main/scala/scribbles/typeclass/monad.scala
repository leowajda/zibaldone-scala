package com.zibaldone
package scribbles.typeclass

import cats.Monad
import cats.instances.list.*
import cats.syntax.applicative.* // .pure
import cats.syntax.flatMap.*     // .flatMap
import cats.syntax.functor.*     // .map

import scala.annotation.tailrec

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

type ErrorOr[T] = Either[Throwable, T] // also a monad

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

@main def monad: List[Int] = 42.pure[List].flatMap(n => n :: n + 1 :: Nil)
