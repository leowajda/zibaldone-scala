package com.zibaldone
package scribbles.advanced

import cats.{Invariant, Show}
import cats.instances.string.*
import cats.syntax.invariant.*

@main def invariant: Show[Option[String]] =

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

  val showString: Show[String]            = Show[String]
  val optShowString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))
  showString.imap(Option(_))(_.getOrElse(""))
