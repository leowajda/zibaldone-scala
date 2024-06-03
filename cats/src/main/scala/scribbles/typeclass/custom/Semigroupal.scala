package com.zibaldone
package scribbles.typeclass.custom

// monad is a semigroupal where the product is equal to a cartesian product
trait Semigroupal[F[_]]:
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
