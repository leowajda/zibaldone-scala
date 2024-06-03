package com.zibaldone
package scribbles.typeclass

import cats.Apply
import cats.instances.option.*
import cats.syntax.apply.*

@main def apply: Option[Int] =

  Apply[Option].ap[Int, String](Some(_.toString))(Some(42))
  
  (Option(20), Option(10), Option(12))
    .tupled         // Option(Int, Int, Int)
    .map(_ + _ + _) // Option(Int)
