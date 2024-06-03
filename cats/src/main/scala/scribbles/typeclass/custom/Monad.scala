package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Monad[F[_]] extends FlatMap[F] with Applicative[F]:
  self =>

  def pure[A](value: A): F[A]

  override def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  override def map[A, B](value: F[A])(f: A => B): F[B] = self.flatMap(value)(a => pure(f(a)))

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    self.flatMap(fa)(a => self.map(fb)(b => (a, b)))
