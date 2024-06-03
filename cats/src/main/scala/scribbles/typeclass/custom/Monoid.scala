package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Monoid[A] extends Semigroup[A]:

  override def combine(a: A, b: A): A

  def empty: A
