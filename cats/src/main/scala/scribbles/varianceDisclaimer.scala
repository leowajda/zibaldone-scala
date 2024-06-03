package com.zibaldone
package scribbles

import cats.instances.int.*
import cats.instances.option.*
import cats.syntax.eq.*

@main def varianceDisclaimer: Boolean =

  sealed trait Vet[-T]
  sealed trait Cage[+T]
  sealed trait Animal // Cat <: Animal
  final class Cat extends Animal

  object Vet:

    final def apply[T]: Vet[T]  = new Vet[T] {}
    final def cure[T: Vet]: Int = 42

  object Cage:

    final def apply[T]: Cage[T]     = new Cage[T] {}
    final def confine[T: Cage]: Int = 42

  object Animal:

    given Cage[Animal] = Cage[Animal]
    given Vet[Animal]  = Vet[Animal]

  object Cat:
    given Cage[Cat] = Cage[Cat]

  val _: Cage[Animal] = Cage[Cat] // covariant in type: Cage[Cat] <: Cage[Animal]
  Cage.confine[Cat]    // Cage[Cat] == Cage[Cat]
  Cage.confine[Animal] // (Cage[Animal] == Cage[Animal]) || (Cage[Cat] <: Cage[Animal])

  val _: Vet[Cat] = Vet[Animal] // contravariant in type: Vet[Animal] <: Vet[Cat]
  Vet.cure[Animal] // Vet[Animal] == Vet[Animal]
  Vet.cure[Cat]    // Vet[Animal] <: Vet[Cat]

  // doesn't compile: Some(42) === None
  Option(42) === Option.empty // invariance for the win
