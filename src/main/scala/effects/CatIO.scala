package effects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import scala.concurrent.duration._
import scala.io.Source
import cats.syntax.all._
import scala.concurrent.Future

object Main extends App:
  val hw: IO[Unit] = IO.delay(println("Hello world!"))
  val hw2: IO[Unit] = IO(println("Hello world!")) // IO.apply == IO.delay

  val ohNoes: IO[Int] = IO(throw new RuntimeException("oh noes!"))

  val twelve: IO[Int] = IO.pure(42)
  // val notPure: IO[Unit] = IO.pure(System.out.println("sajt"))

  val ohNoes2: IO[Int] =
    if false
    then IO.raiseError(new RuntimeException("oh noes!"))
    else IO.pure(12)

  ohNoes2.unsafeRunSync()

  val fut: IO[String] = IO.fromFuture(IO(futurish))
  def futurish: Future[String] = ???

  // fut.unsafeRunSync()

  IO.pure(25).map(_ + 1).flatMap(IO.println(_)).unsafeRunSync()

  (IO((a: Int) => a + 1) <*> IO(2) >>= (a => IO.println(a))).unsafeRunSync()
  IO((a: Int) => a + 1).ap(IO(2)).flatMap(IO.println(_)).unsafeRunSync()

  (IO(12), IO("hi")).mapN((i, s) => s"$s: $i")

  val x = for
    i <- IO(12)
    j <- IO(i + 1)
  yield j

  (x >>= (IO.println(_))).unsafeRunSync()
  x.as(4).flatMap(IO.println(_)).unsafeRunSync()
