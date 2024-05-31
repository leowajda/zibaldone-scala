package com.zibaldone
package playground.typeclass

package eq:

  package custom:

    trait Eq[A]:

      def eqv(x: A, y: A): Boolean
      final def neqv(x: A, y: A): Boolean = !eqv(x, y)

  private[eq] case object example:

    import cats.Eq
    import cats.syntax.eq.*
    import cats.instances.int.*
    import cats.instances.list.*

    (42 :: Nil) === (42 :: Nil)
    (24 :: Nil) =!= (42 :: Nil)

    final case class ToyCar(
      modelName: String,
      price    : Double
    )

    object ToyCar:

      given eq(using Eq[Double]): Eq[ToyCar] = Eq.instance(_.price === _.price)

    ToyCar("a", 24.0) === ToyCar("b", 42.0)
