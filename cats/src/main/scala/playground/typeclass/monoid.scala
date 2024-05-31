package com.zibaldone
package playground.typeclass

package monoid:

  package custom:

    trait Monoid[A] extends semigroup.custom.Semigroup[A]:

      override def combine(a: A, b: A): A
      def empty: A

  private[monoid] case object example:

    import cats.Monoid
    import cats.syntax.monoid.*

    // semigroup.reduce[Int](Nil) throws java.lang.UnsupportedOperationException
    // no Monoid provided to prevent this from happening
    final def reduceV2[A](using monoid: Monoid[A])(coll: Iterable[A]): A =
      coll.foldLeft(monoid.empty)(_ |+| _)

    final case class ShoppingCart(itemIds: Set[String])

    object ShoppingCart:

      given shoppingCartMonoid(using monoid: Monoid[Set[String]]): Monoid[ShoppingCart] =
        Monoid.instance(
          emptyValue = ShoppingCart(monoid.empty),
          cmb = (a, b) => ShoppingCart(a.itemIds |+| a.itemIds)
        )

      final def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
        reduceV2[ShoppingCart](shoppingCarts)
