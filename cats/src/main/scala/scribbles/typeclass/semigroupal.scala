package com.zibaldone
package scribbles.typeclass

import cats.Monad
import cats.Semigroupal
import cats.data.Validated
import cats.instances.option.*
import cats.instances.future.*
import cats.instances.list.*
import cats.syntax.flatMap.* // .flatMap
import cats.syntax.functor.* // .map

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@main def semigroupal: Either[List[String], (String, String)] =

  Semigroupal[Option].product[Int, Int](Some(42), None)            // None
  Semigroupal[Future].product(Future("meaningOfLife"), Future(42)) // Future(("meaningOfLife", 42))

  Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, "a"), (1, "b"), (2, "a") (2, "b"))

  def monadicProduct[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for a <- fa; b <- fb yield (a, b)

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
