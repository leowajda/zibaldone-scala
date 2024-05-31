package com.zibaldone
package playground.data

package eval:

  private[eval] case object example:

    import cats.Eval

    Eval.now(42)    // val
    Eval.always(42) // def
    Eval.later(42)  // lazy val

    val meaningOfLife: Eval[Int] =
      for
        a <- Eval.now(20)
        b <- Eval.always(20).memoize
        c <- Eval.later(1)
        d <- Eval.always(1)
      yield a + b + c + d

    final def artificiallyDefer[T](eval: => Eval[T]): Eval[T] =
      Eval.later(()).flatMap(_ => eval)

    artificiallyDefer:
      Eval.now:
        println("hello world!")
        42

    // deferring evals is stack safe!
    final def dummyListReversal[T](list: List[T]): Eval[List[T]] =
      list match
        case Nil          => Eval.now(Nil)
        case head :: rest => Eval.defer(dummyListReversal(rest).map(_ :+ head))
