package effects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import cats.effect.implicits._
import scala.concurrent.duration._
import scala.concurrent._
import scala.io.Source
import cats.syntax.all._
import cats.effect.kernel.Deferred
import java.util.concurrent.Executors
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.CompletableFuture
import java.io.RandomAccessFile

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

object Errors extends App:
  val ohNoes = IO.raiseError[Int](new RuntimeException("oh noes!"))

  val handled: IO[Int] =
    ohNoes.handleError(_ => 2) // Short-circuit, provide value then stop

  val handled2: IO[Int] =
    ohNoes.handleErrorWith(_ => IO(2)) // Continue computation

  handled.flatMap(IO.println(_)).unsafeRunSync()
  handled2.flatMap(IO.println(_)).unsafeRunSync()

  val adapt: IO[Int] =
    ohNoes.adaptError(t => new Error("oh noo!"))

  // val attempted: IO[Either[Throwable, Int]] =
  //   ohNoes
  //     .map(i => Right(i): Either[Throwable, Int])
  //     .handleErrorWith(t => Left(t))
  val attempted: IO[Either[Throwable, Int]] =
    ohNoes.attempt
  // Puts the result into a Left if there was an exception,
  // or a Right if it succeeded

  val ohNoes2 = IO.pure(2)
  val attempted2: IO[Either[Throwable, Int]] =
    ohNoes2.attempt

  attempted2.flatMap(IO.println(_)).unsafeRunSync()

  // onError performs an effect, but lets the error through
  ohNoes
    .onError(t => IO.println("sajt"))
    .handleError(_ => IO(2))
    .unsafeRunSync()

object Execute extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    someComputation.as(ExitCode.Success)

  def someComputation: IO[Unit] = for
    _ <- IO.println("hello")
    a = IO.raiseError[Unit](new RuntimeException("nooo!"))
    ab <- a.attempt
  yield ()
