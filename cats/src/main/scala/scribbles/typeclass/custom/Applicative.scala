package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Applicative[F[_]] extends Apply[F]:
  def pure[A](a: A): F[A]
