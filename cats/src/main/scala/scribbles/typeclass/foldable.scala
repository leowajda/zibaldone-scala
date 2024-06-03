package com.zibaldone
package scribbles.typeclass

import cats.Foldable
import cats.Eval
import cats.instances.list.*
import cats.instances.string.*
import cats.instances.vector.*
import cats.syntax.foldable.*

@main def foldable: String =

  val nums: List[Int] = (1 to 100).toList

  Foldable[List].foldLeft(nums, 0)(_ + _)
  Foldable[List].foldRight(nums, Eval.now(0)) { (elem, eval) => eval.map(_ + elem) }
  Foldable[List].combineAll(nums)
  Foldable[List].foldMap(nums)(_.toString)

  (Foldable[Vector] compose Foldable[List]).combineAll(Vector(nums, Nil))

  nums.combineAll
  nums.foldMap(_.toString)
