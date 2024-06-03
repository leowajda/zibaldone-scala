package com.zibaldone
package scribbles.typeclass.custom

import cats.Eval

private[custom] trait Foldable[F[_]]:

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  // .foldRight is stack-safe regardless of F[A]
  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
