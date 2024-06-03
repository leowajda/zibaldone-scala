package com.zibaldone
package scribbles.typeclass

import cats.data.Validated
import cats.instances.list.*
import cats.instances.option.*
import cats.syntax.applicative.*

@main def applicative: Validated[List[String], Int] =
  42.pure[List] // List(1)
  42.pure[Option]

  type ValidatedErrorOr[T] = Validated[List[String], T]
  // Validated cannot be considered a Monad, but it's still a valid Applicative.
  1.pure[ValidatedErrorOr]
