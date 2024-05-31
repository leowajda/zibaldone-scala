package com.zibaldone
package playground.advanced

package invariant:

  package custom:

    trait Invariant[F[_]]:
      def imap[A, B](fa: F[A])(fourth: A => B)(back: B => A): F[B]

  private[invariant] case object example:

    trait Crypto[A]:
      self =>

      def encrypt(value: A): String
      def decrypt(value: String): A

      final def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B]:

        override def encrypt(value: B): String = self.encrypt(back(value))
        override def decrypt(value: String): B = forth(self.decrypt(value))

    object Crypto:

      def encrypt[A](value: A)(using crypto: Crypto[A]): String = crypto.encrypt(value)
      def decrypt[A](value: String)(using crypto: Crypto[A]): A = crypto.decrypt(value)

      given ceasarCypher: Crypto[String] = new Crypto[String]:

        override def decrypt(value: String): String = value.map(c => (c - 2).toChar)
        override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

      given Crypto[Double] = ceasarCypher.imap(_.toString, _.toDouble)

    import cats.Invariant
    import cats.Show
    import cats.instances.string.*

    val showString: Show[String]            = Show[String]
    val optShowString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

    import cats.syntax.invariant.*
    showString.imap(Option(_))(_.getOrElse(""))
