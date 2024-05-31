package com.zibaldone
package playground.typeclass

package apply:

  package custom:

    trait Apply[F[_]] extends functor.custom.Functor[F] with semigroupal.custom.Semigroupal[F]:
      self =>

      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        val funcWrapper: F[B => (A, B)] = self.map(fa)(a => (b: B) => (a, b))
        self.ap(funcWrapper)(fb)

      def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
        val unwrapped: F[(A, B)] = self.product(tuple._1, tuple._2)
        self.map[(A, B), C](unwrapped)((a, b) => f(a, b))

      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  private[apply] case object example:

    import cats.Apply
    import cats.instances.option.*

    Apply[Option].ap[Int, String](Some(_.toString))(Some(42))

    import cats.syntax.apply.*

    (Option(20), Option(10), Option(12))
      .tupled         // Option(Int, Int, Int)
      .map(_ + _ + _) // Option(Int)
