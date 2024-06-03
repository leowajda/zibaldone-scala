package com.zibaldone
package scribbles.typeclass.custom

private[custom] trait Eq[A]:

  def eqv(x: A, y: A): Boolean
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
