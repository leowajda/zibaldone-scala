package com.zibaldone
package scribbles.typeclass

import cats.Monad
import cats.Traverse
import cats.Applicative
import cats.data.Validated
import cats.instances.vector.*
import cats.instances.future.*
import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@main def traverse: Validated[List[String], List[Int]] =
  val servers: List[String]                    = Nil
  def bandwidth(hostName: String): Future[Int] = Future(42)

  Future.traverse(servers)(bandwidth)         // Future[List[Int]]
  Future.sequence(servers.map(bandwidth))     // Future[List[Int]]
  Traverse[List].traverse(servers)(bandwidth) // Future[List[Int]]
  servers.traverse(bandwidth)                 // Future[List[Int]]

  def monadTraverse[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure) { (acc, elem) => for x <- acc; y <- f(elem) yield x :+ y }

  // looser bound, can also be used on instances of Validated
  def applicativeTraverse[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure) { (acc, elem) => (acc, f(elem)).mapN(_ :+ _) }

  def applicativeSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    applicativeTraverse(list)(identity)

  // Vector[List[Int]] = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  applicativeSequence(Vector(1, 2) :: Vector(3, 4) :: Nil)

  def filterEvensAsOption(list: List[Int])(f: Int => Boolean): Option[List[Int]] =
    applicativeTraverse(list)(n => Option(n).filter(_ % 2 == 0))

  // Option[List[Int]] = Some(List(2, 4, 6))
  filterEvensAsOption(2 :: 4 :: 6 :: Nil)

  // Option[List[Int]] = None
  filterEvensAsOption(1 :: 4 :: 6 :: Nil)

  type ErrorsOr[T] = Validated[List[String], T]

  def filterEvensAsValidated(list: List[Int]): ErrorsOr[List[Int]] =
    applicativeTraverse(list) { n => Validated.cond(n % 2 == 0, n, List(s"predicate failed for $n")) }

  // Validated[List[String], List[Int]] = Validated(List(2, 4, 6))
  filterEvensAsValidated(2 :: 4 :: 6 :: Nil)

  // Validated[List[String], List[Int]] = Invalid(List("predicate failed for 1")) -- Semigroupal[Validated]
  filterEvensAsValidated(1 :: 4 :: 6 :: Nil)
