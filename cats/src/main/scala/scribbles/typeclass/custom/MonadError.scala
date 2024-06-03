package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]:

  def ensure[A](fa: F[A])(err: => E)(predicate: E => Boolean): F[A]
