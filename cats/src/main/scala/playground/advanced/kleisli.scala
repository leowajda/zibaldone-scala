package com.zibaldone
package playground.advanced

object kleisli:

  private[kleisli] case object example:

    // chaining functions that return higher-kinded types is hard
    val a: Int => Option[Int]    = num => Option(num)
    val b: Int => Option[String] = num => if num % 2 == 0 then Some(s"$num is even") else None

    import cats.data.Kleisli
    import cats.instances.option.* // FlatMap[Option]

    val aK: Kleisli[Option, Int, Int]    = Kleisli(a)
    val bK: Kleisli[Option, Int, String] = Kleisli(b)

    val abK: Kleisli[Option, Int, String] = aK andThen bK

    abK
      .map(_.toUpperCase)
      .flatMap(s => Kleisli[Option, Int, String](n => Option(s * n)))
      .apply(2)

    // cats.Reader is implemented in terms of Kleisli[Id, A, B] so it generalizes dependency injection
    import cats.Id

    type Reader[A, B] = Kleisli[Id, A, B]
