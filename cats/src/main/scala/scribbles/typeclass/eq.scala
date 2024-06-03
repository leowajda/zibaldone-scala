package com.zibaldone
package scribbles.typeclass

import cats.Eq
import cats.instances.int.*
import cats.instances.list.*
import cats.syntax.eq.*

final case class ToyCar(
  modelName: String,
  price    : Double
)

object ToyCar:
  given eq(using Eq[Double]): Eq[ToyCar] = Eq.instance(_.price === _.price)

@main def eq: Boolean =

  (42 :: Nil) === (42 :: Nil)
  (24 :: Nil) =!= (42 :: Nil)

  ToyCar("a", 24.0) === ToyCar("b", 42.0)
