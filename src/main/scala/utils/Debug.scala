package Utils.Debug.syntax

import cats.effect.*


val clock: IO[Long] = IO(System.currentTimeMillis())

extension [A](io: IO[A])
  def t: IO[A] = for
    before <- clock
    res <- io
    after <- clock
    duration = after - before
    _ <- IO(println(s"took $duration"))
  yield res

  def d: IO[A] =
    io.flatTap { a =>
      IO(println(s"[${Thread.currentThread.getName}] $a"))
    }
