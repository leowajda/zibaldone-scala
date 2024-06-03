package com.zibaldone
package scribbles.typeclass.custom

// also called the covariant functor
private[custom] trait Functor[F[_]] extends Invariant[F]:
  self => 

  def map[A, B](value: F[A])(f: A => B): F[B]

  override def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B] =
    self.map(fa)(fourth)
