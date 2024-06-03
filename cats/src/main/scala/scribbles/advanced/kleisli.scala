package com.zibaldone
package scribbles.advanced

import cats.Id
import cats.data.Kleisli
import cats.instances.option.* // FlatMap[Option]

@main def kleisli: Option[String] =
  // chaining functions that return higher-kinded types is hard
  val a: Int => Option[Int]    = num => Option(num)
  val b: Int => Option[String] = num => if num % 2 == 0 then Some(s"$num is even") else None

  val aK: Kleisli[Option, Int, Int]    = Kleisli(a)
  val bK: Kleisli[Option, Int, String] = Kleisli(b)

  // cats.Reader is implemented in terms of Kleisli[Id, A, B]
  type Reader[A, B] = Kleisli[Id, A, B]

  val abK: Kleisli[Option, Int, String] = aK andThen bK
  abK
    .map(_.toUpperCase)
    .flatMap(s => Kleisli[Option, Int, String](n => Option(s * n)))
    .apply(2)
