package com.zibaldone
package playground.typeclass

package foldable:

  package custom:

    import cats.Eval

    trait Foldable[F[_]]:

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

      // .foldRight is stack-safe regardless of F[A]
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  private[foldable] case object example:

    import cats.Foldable
    import cats.Eval
    import cats.instances.list.*
    import cats.instances.string.*

    val nums: List[Int] = (1 to 100).toList

    Foldable[List].foldLeft(nums, 0)(_ + _)
    Foldable[List].foldRight(nums, Eval.now(0)) { (elem, eval) => eval.map(_ + elem) }
    Foldable[List].combineAll(nums)
    Foldable[List].foldMap(nums)(_.toString)

    import cats.instances.vector.*
    (Foldable[Vector] compose Foldable[List]).combineAll(Vector(nums, Nil))

    import cats.syntax.foldable.*
    nums.combineAll
    nums.foldMap(_.toString)
