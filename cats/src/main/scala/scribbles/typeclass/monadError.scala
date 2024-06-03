package com.zibaldone
package scribbles.typeclass

import cats.MonadError
import cats.ApplicativeError
import cats.data.Validated
import cats.instances.either.*
import cats.instances.try_.*          // MonadThrow[Try]
import cats.instances.future.*        // MonadThrow[Future]
import cats.instances.vector.*
import cats.syntax.applicative.*      // .pure
import cats.syntax.applicativeError.* // .raiseError .handleError .handleErrorWith
import cats.syntax.monadError.*       // .ensure

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

@main def monadError: Either[String, Int] =

  type ErrorOr[T] = Either[String, T]
  val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val failure: ErrorOr[Int]                   = monadError.raiseError[Int]("something wrong")
  val pure: ErrorOr[Int]                      = monadError.pure[Int](42)

  monadError.handleError(failure)(_ => 42)
  monadError.handleErrorWith(failure)(_ => pure)
  monadError.ensure(pure)("not meaning of life")(_ == 42)

  MonadError[Try, Throwable].raiseError[Nothing](new RuntimeException) // Failure(new RuntimeException)
  MonadError[Future, Throwable].pure[Int](42)

  type ValidateOr[T] = Validated[Vector[Throwable], T]
  ApplicativeError[ValidateOr, Vector[Throwable]]

  42.pure[ValidateOr]
  Vector(new RuntimeException).raiseError[ValidateOr, Int]
  pure.ensure("not meaning of life")(_ == 42)
