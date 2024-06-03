package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Semigroup[A]:
  def combine(a: A, b: A): A
