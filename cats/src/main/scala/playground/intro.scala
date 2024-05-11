package com.zibaldone
package playground

case object intro:

  import cats.Eq              // type class definition
  import cats.instances.int._ // type class instances

  Eq[Int].eqv(42, 42)
  Eq[Int].neqv(24, 42)

  import cats.syntax.eq._ // type class extension methods

  42 === 42
  24 =!= 42

  import cats.instances.list._ // type class for composite types

  (42 :: Nil) === (42 :: Nil)
  (24 :: Nil) =!= (42 :: Nil)

  final case class ToyCar(
    modelName: String,
    price:     Double
  )

  object ToyCar:

    given eq(using Eq[Double]): Eq[ToyCar] = // type class definition for custom types
      Eq.instance(_.price === _.price)

  ToyCar("a", 24.0) === ToyCar("b", 42.0)
