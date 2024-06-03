package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait FlatMap[F[_]] extends Apply[F]:
  self =>

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    self.flatMap(ff)(f => self.map(fa)(a => f(a)))
