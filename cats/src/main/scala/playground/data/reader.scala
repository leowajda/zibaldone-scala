package com.zibaldone
package playground.data

package reader:

  private[reader] case object example:

    final case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host      : String,
      port      : Int,
      nThreads  : Int
    )

    import cats.data.Reader // dependency injection in the FP style

    final case class OrderService private[OrderService] (host: String, port: Int, nThreads: Int):

      def dbSettings: String = ???

    object OrderService:

      def apply(conf: Configuration): Reader[Configuration, OrderService] = Reader: conf =>
        OrderService(conf.host, conf.port, conf.nThreads)

      def debug(conf: Configuration): Reader[Configuration, Unit] =
        val orderServiceReader = OrderService(conf)

        for // readers are composable
          orderService <- orderServiceReader
          settings     <- orderServiceReader.map(_.dbSettings)
        yield println(settings)
