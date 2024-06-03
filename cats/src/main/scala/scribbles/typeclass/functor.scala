package com.zibaldone
package scribbles.typeclass

import cats.Functor
import cats.syntax.functor.*

sealed trait Tree[+T]
final case class Leaf[+T](value: T)                     extends Tree[T]
final case class Node[+T](l: Tree[T], v: T, r: Tree[T]) extends Tree[T]

object Tree:

  def leaf[T](value: T): Tree[T]                         = Leaf(value)
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

@main def functor: Tree[Int] = Tree.leaf(21).fmap(_ * 2)
