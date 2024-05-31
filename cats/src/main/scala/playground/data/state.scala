package com.zibaldone
package playground.data

package state:

  package custom:

    type State[S, A] = S => (S, A)

  private[state] case object example:

    import cats.data.State // defined as StateT[Eval, S, A]

    var a                 = 10
    a += 1
    val firstComputation  = s"a = ${10} + ${1}"
    a *= 5
    val secondComputation = s"a = ${11} * ${5}"

    val stateComputation: State[Int, (String, String)] =
      for // state is an abstraction for an iterative computation
        a <- State((num: Int) => (num + 1, s"num = $num + ${1}"))
        b <- State((num: Int) => (num * 5, s"num = $num * ${5}"))
      yield (a, b)

    val (result, (firstOutput, secondOutput)) = stateComputation.run(10).value

    final def inspect[A, B](f: A => B): State[A, B] = State(s => (s, f(s)))
    final def set[A](value: A): State[A, Unit]      = State(_ => (value, ()))
    final def modify[A](f: A => A): State[A, Unit]  = State(s => (f(s), ()))
    final def get[A]: State[A, A]                   = State(s => (s, s))

    for // imperative computation reduced to pure functional programming
      a <- get[Int]
      _ <- set[Int](a + 10)
      b <- get[Int]
      _ <- modify[Int](_ + 43)
      c <- inspect[Int, Int](_ * 2)
    yield (a, b, c)
