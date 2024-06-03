package com.zibaldone
package scribbles.data

import cats.Semigroup
import cats.data.Validated
import cats.instances.vector.*

import scala.annotation.tailrec
import Math.abs

def isPrime(num: Int): Boolean                               =
  @tailrec def tailRecIsPrime(d: Int): Boolean =
    if d <= 1 then true else num % d != 0 && tailRecIsPrime(d - 1)
  if num == 0 || num == 1 || num == -1 then false else tailRecIsPrime(abs(num / 2))

// only '2' satisfies all the conditions
def eitherSpecialNumber(num: Int): Either[List[String], Int] =
  val isPrimeNum    = if isPrime(num) then List.empty[String] else List(s"$num must be a prime.")
  val isPositiveNum = if num >= 0 then List.empty[String] else List(s"$num must be positive.")
  val isWithinRange = if num <= 100 then List.empty[String] else List(s"$num must be <= 100.")
  val isEvenNum     = if num % 2 == 0 then List.empty[String] else List(s"$num must be even.")

  Either.cond(
    test = isPrime(num) && num >= 0 && num <= 100 && num % 2 == 0,
    right = num,
    left = isPrimeNum ++ isPositiveNum ++ isWithinRange ++ isEvenNum
  )

def validatedSpecialNumber(num: Int): Validated[Vector[String], Int] =
  given Semigroup[Int] = Semigroup.first // same number

  val isPrimeNum    = Validated.cond(isPrime(num), num, Vector(s"$num must be a prime."))
  val isPositiveNum = Validated.cond(num >= 0, num, Vector(s"$num must be positive."))
  val isWithinRange = Validated.cond(num <= 100, num, Vector(s"$num must be <= 100."))
  val isEvenNum     = Validated.cond(num % 2 == 0, num, Vector(s"$num must be even."))

  isPrimeNum combine isPositiveNum combine isWithinRange combine isEvenNum

object FormValidation:

  private[FormValidation] given Semigroup[String] = Semigroup.first

  type FormValidation[T] = Validated[Vector[String], T]

  private[FormValidation] final def get(rawForm: Map[String, String], fieldName: String): FormValidation[String] =
    Validated.fromOption(rawForm.get(fieldName), Vector(s"$fieldName is not present."))

  final def apply(inputs: Map[String, String]): FormValidation[String] =

    val nameValidation: FormValidation[String] =
      get(inputs, "name").ensure(Vector("name must not be blank"))(s => !s.isBlank)

    val passwordValidation: FormValidation[String] = get(inputs, "password")
      .ensure(Vector("password is too short"))(_.length >= 20)

    val emailValidation: FormValidation[String] = get(inputs, "email")
      .ensure(Vector("invalid email"))(_.contains('@'))

    (nameValidation combine passwordValidation combine emailValidation).map(_ => "all good")

@main def validated: Validated[String, Int] =
  Validated.fromOption[String, Int](None, "empty")
    .map(_ + 20)
    .leftMap(_.toUpperCase)
    .bimap(_.toLowerCase, _ + 20)
    .ensure("something bad")(_ == 42)
    .andThen(num => Validated.cond(num + 2 == 42, 42, "bad"))
