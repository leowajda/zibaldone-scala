package com.zibaldone
package scribbles.typeclass

import cats.data.OptionT
import cats.data.EitherT
import cats.instances.future.*

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

type AsyncResponse[T] = EitherT[Future, String, T]

def bandwidth(serverName: String): AsyncResponse[Int] =
  Map.empty[String, Int].get(serverName) match
    case Some(value) => EitherT.right(Future(value))
    case None        => EitherT.left(Future(s"server $serverName not present"))

def canWithstandSurge(firstServer: String, secondServer: String): AsyncResponse[Boolean] =
  for
    firstBandwidth  <- bandwidth(firstServer)
    secondBandwidth <- bandwidth(secondServer)
  yield firstBandwidth + secondBandwidth > 250

def generateReport(firstServer: String, secondServer: String): AsyncResponse[String] =
  canWithstandSurge(firstServer, secondServer).transform:
    case Left(value)  => Left(value)
    case Right(value) => Right(s"""servers ${if value then "can" else "cannot"} withstand the traffic surge.""")

@main def monadTransformer: List[Option[Int]] = OptionT[List, Int](None :: Option(2) :: Option(40) :: Nil).value
