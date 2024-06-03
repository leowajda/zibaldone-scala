package com.zibaldone
package scribbles.typeclass

import cats.Monoid
import cats.syntax.monoid.*

// semigroup.reduce[Int](Nil) throws java.lang.UnsupportedOperationException
// no Monoid provided to prevent this from happening
def reduceV2[A](using monoid: Monoid[A])(coll: Iterable[A]): A =
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
