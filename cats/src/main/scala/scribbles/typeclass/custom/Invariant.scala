package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Invariant[F[_]]:
  def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B]
