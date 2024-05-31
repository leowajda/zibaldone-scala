package com.zibaldone
package playground.typeclass

package monadTransformer:

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  // provides .map and .flatMap for monads wrapped into other monads
  private[monadTransformer] case object example:

    import cats.instances.list.*
    import cats.instances.future.*

    import cats.data.OptionT

    OptionT[List, Int](None :: Option(2) :: Option(40) :: Nil)

    import cats.data.EitherT
    EitherT[List, String, Int](Left("") :: Right(42) :: Nil)

    val bandwidths: Map[String, Int] = Map(
      "server1" -> 40,
      "server2" -> 150,
      "server3" -> 100
    )

    type AsyncResponse[T] = EitherT[Future, String, T]

    final def bandwidth(serverName: String): AsyncResponse[Int] =
      bandwidths.get(serverName) match
        case Some(value) => EitherT.right(Future(value))
        case None        => EitherT.left(Future(s"server $serverName not present"))

    final def canWithstandSurge(firstServer: String, secondServer: String): AsyncResponse[Boolean] =
      for
        firstBandwidth  <- bandwidth(firstServer)
        secondBandwidth <- bandwidth(secondServer)
      yield firstBandwidth + secondBandwidth > 250

    final def generateReport(firstServer: String, secondServer: String): AsyncResponse[String] =
      canWithstandSurge(firstServer, secondServer).transform:
        case Left(value)  => Left(value)
        case Right(value) => Right(s"""servers ${if value then "can" else "cannot"} withstand the traffic surge.""")
