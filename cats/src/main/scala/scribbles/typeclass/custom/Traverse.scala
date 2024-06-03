package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Traverse[L[_]] extends Foldable[L] with Functor[L]:
  self =>

  def traverse[F[_]: Applicative, A, B](lfa: L[A])(f: A => F[B]): F[L[B]]

  def sequence[F[_]: Applicative, A](lfa: L[F[A]]): F[L[A]] =
    self.traverse(lfa)(identity)

  type Id[T] = T

  given Applicative[Id] = new Applicative[Id]:

    override def pure[A](a: A): Id[A]                       = a
    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)
    override def map[A, B](value: Id[A])(f: A => B): Id[B]  = f(value)

  override def map[A, B](la: L[A])(f: A => B): L[B] =
    self.traverse[Id, A, B](la)(f)
