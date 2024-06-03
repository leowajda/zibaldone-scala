package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait ApplicativeError[F[_], E] extends Applicative[F]:
  self =>

  def raiseError[A](err: E): F[A]

  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  def handleError[A](fa: F[A])(f: E => A): F[A] =
    self.handleErrorWith(fa)(e => self.pure(f(e)))
