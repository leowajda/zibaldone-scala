package com.zibaldone
package playground.typeclass

package functor:

  package custom:

    // also called the covariant functor
    trait Functor[F[_]] extends playground.advanced.invariant.custom.Invariant[F]:
      self =>

      def map[A, B](value: F[A])(f: A => B): F[B]

      override def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B] =
        self.map(fa)(fourth)

  private[functor] case object example:

    import cats.Eq
    import cats.Functor
    import cats.syntax.functor.*
    import cats.instances.list.*
    import cats.syntax.eq.*

    Functor[List].map[Int, Int](40 :: Nil)(_ + 1) === List(40).fmap(_ + 1)

    sealed trait Tree[+T]
    final case class Leaf[+T](value: T)                     extends Tree[T]
    final case class Node[+T](l: Tree[T], v: T, r: Tree[T]) extends Tree[T]

    object Tree:

      def leaf[T](value: T): Tree[T] = Leaf(value)

      def node[T](l: Tree[T], value: T, r: Tree[T]): Tree[T] = Node(l, value, r)

      given Functor[Tree] = new Functor[Tree]:
        self =>

        override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
          case Leaf(value)              => Leaf(f(value))
          case Node(left, value, right) => Node(
              l = self.map(left)(f),
              v = f(value),
              r = self.map(right)(f)
            )

    Tree.leaf(21).fmap(_ * 2)
