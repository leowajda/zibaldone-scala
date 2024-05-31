package com.zibaldone
package playground.typeclass

package traverse:

  package custom:

    trait Traverse[L[_]] extends foldable.custom.Foldable[L] with functor.custom.Functor[L]:
      self =>

      def traverse[F[_]: applicative.custom.Applicative, A, B](lfa: L[A])(f: A => F[B]): F[L[B]]

      def sequence[F[_]: applicative.custom.Applicative, A](lfa: L[F[A]]): F[L[A]] =
        self.traverse(lfa)(identity)

      type Id[T] = T

      given applicative.custom.Applicative[Id] =
        new applicative.custom.Applicative[Id]:

          override def pure[A](a: A): Id[A]                       = a
          override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)
          override def map[A, B](value: Id[A])(f: A => B): Id[B]  = f(value)

      override def map[A, B](la: L[A])(f: A => B): L[B] = self.traverse[Id, A, B](la)(f)

  private[traverse] case object example:

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import cats.instances.future.*

    val servers: List[String]                          = Nil
    final def bandwidth(hostName: String): Future[Int] = ???

    Future.traverse(servers)(bandwidth)     // Future[List[Int]]
    Future.sequence(servers.map(bandwidth)) // Future[List[Int]]

    import cats.Traverse
    import cats.syntax.traverse.*

    Traverse[List].traverse(servers)(bandwidth) // Future[List[Int]]
    servers.traverse(bandwidth)                 // Future[List[Int]]

    import cats.Monad
    import cats.syntax.flatMap.*
    import cats.syntax.functor.*
    import cats.syntax.applicative.*

    import cats.Applicative
    import cats.syntax.apply.*

    final def monadTraverse[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure) { (acc, elem) => for x <- acc; y <- f(elem) yield x :+ y }

    // looser bound, can also be used on instances of Validated
    final def applicativeTraverse[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure) { (acc, elem) => (acc, f(elem)).mapN(_ :+ _) }

    final def applicativeSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
      applicativeTraverse(list)(identity)

    import cats.instances.vector.*

    // Vector[List[Int]] = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    applicativeSequence(Vector(1, 2) :: Vector(3, 4) :: Nil)

    final def filterEvensAsOption(list: List[Int])(f: Int => Boolean): Option[List[Int]] =
      applicativeTraverse(list)(n => Option(n).filter(_ % 2 == 0))

    // Option[List[Int]] = Some(List(2, 4, 6))
    filterEvensAsOption(2 :: 4 :: 6 :: Nil)

    // Option[List[Int]] = None
    filterEvensAsOption(1 :: 4 :: 6 :: Nil)

    import cats.data.Validated
    type ErrorsOr[T] = Validated[List[String], T]

    final def filterEvensAsValidated(list: List[Int])(f: Int => Boolean): ErrorsOr[List[Int]] =
      applicativeTraverse(list) { n => Validated.cond(n % 2 == 0, n, List(s"predicate failed for $n")) }

    // Validated[List[String], List[Int]] = Validated(List(2, 4, 6))
    filterEvensAsValidated(2 :: 4 :: 6 :: Nil)

    // Validated[List[String], List[Int]] = Invalid(List("predicate failed for 1")) -- Semigroupal[Validated]
    filterEvensAsValidated(1 :: 4 :: 6 :: Nil)
