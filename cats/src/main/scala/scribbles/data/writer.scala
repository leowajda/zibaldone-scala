package com.zibaldone
package scribbles.data

import cats.data.Writer
import cats.instances.vector.* // .flatMap requires a Semigroup over the log type

@main def writer: Writer[Vector[String], Int] =
  val (logs, value) =
    Writer(Seq.empty[String], 40) // logs transformation over the provided value
      .map(_ + 1)                 // modifies value
      .mapWritten(_ :+ "modifies log")
      .bimap(_ +: "changes log and value", _ + 1)
      .mapBoth((logs, value) =>
        (logs :+ s"value mapped from $value to ${value * 2}", value * 2)
      )
      .run

  val meaningOfLife: Writer[Vector[String], Int] = // or WriterT[Id, Vector[String], Int]
    for
      a <- Writer(Vector("a"), 40)
      _ <- Writer.tell(Vector("equivalent to println"))
      _ <- Writer.tell(Vector("but separates the side effect from the log"))
      _ <- Writer.tell(Vector("meaning that we can achieve deterministic behavior in a multi-threaded environment"))
      b <- Writer(Vector("b"), 2)
    yield a + b

  meaningOfLife.reset // drops the log and keeps the value, requires a Monoid over the log type
