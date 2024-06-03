package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Contravariant[F[_]] extends Invariant[F]:
  self =>

  def contramap[A, B](fa: F[A])(back: B => A): F[B]

  override def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B] =
    self.contramap(fa)(back)
