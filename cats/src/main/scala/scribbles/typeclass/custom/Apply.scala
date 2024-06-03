package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Apply[F[_]] extends Functor[F] with Semigroupal[F]:
  self =>

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    val funcWrapper: F[B => (A, B)] = self.map(fa)(a => (b: B) => (a, b))
    self.ap(funcWrapper)(fb)

  def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
    val unwrapped: F[(A, B)] = self.product(tuple._1, tuple._2)
    self.map[(A, B), C](unwrapped)((a, b) => f(a, b))

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
