package com.zibaldone
package scribbles.advanced

import cats.Contravariant
import cats.Show
import cats.instances.int.*
import cats.syntax.contravariant.*

@main def contravariant: Show[Option[Int]] =

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

  val showInt: Show[Int]            = Show[Int]
  val optShowInt: Show[Option[Int]] = Contravariant[Show].contramap(showInt)(_.getOrElse(-1))
  showInt.contramap[Option[Int]](_.getOrElse(-1))
