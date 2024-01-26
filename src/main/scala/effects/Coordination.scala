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
import debug.*
import cats.effect.kernel.Ref

object ConcurrentStateVar extends IOApp.Simple:
  def run: IO[Unit] = for
    _ <- (printTicks :: List.fill(3)(tickingClock)).parSequence
    // _ <- (tickingClock, printTicks).parTupled
  yield ()

  var ticks: Long = 0l

  val tickingClock: IO[Unit] = for
    _ <- IO.sleep(1.second)
    _ <- IO(System.currentTimeMillis).myDebug
    _ = (ticks = ticks + 1)
    _ <- tickingClock
  yield ()

  val printTicks: IO[Unit] = for
    _ <- IO.sleep(5.seconds)
    _ <- IO(s"TICKS: $ticks").myDebug.void
    _ <- printTicks
  yield ()

object RefExample extends IOApp.Simple:
  def doSomething(ref: Ref[IO, Int]) = for 
    _ <- ref.modify(i => (i+1, IO))
  yield ()

  def twice(ref: Ref[IO, Int]) = for
    _ <- List.fill(1000)(ref).parTraverse(doSomething(_))
    // _ <- (doSomething(ref), doSomething(ref)).parTupled
  yield ()

  def run: IO[Unit] = for
    ref <- IO.ref(0)
    _ <- twice(ref)
    _ <- ref.get.flatMap(IO.println(_))
  yield ()


object Example5 extends IOApp.Simple:
  def run: IO[Unit] = for
    state <- IO.ref(0) // This works like IORef in Haskell
    fibers <- state.update(_ + 1).start.replicateA(100)
    _ <- fibers.traverse_(_.join)
    value <- state.get
    _ <- IO.println(s"The final value is: $value")
  yield ()

object Example6 extends IOApp.Simple:
  def run: IO[Unit] = for
    state <- IO.deferred[Int] // This works like MVar in Haskell
    f <- state.get.start // Blocks until MVar is filled
    _ <- IO.sleep(2.seconds)
    _ <- state.complete(2) // Fill MVar on main fiber
    a <- f.joinWithNever
    _ <- IO.println(s"The final value is: $a")
  yield ()

object Example7 extends IOApp.Simple:
  def countdown(n: Int, pause: Int, waiter: Deferred[IO, Unit]): IO[Unit] =
    IO.println(n) *> IO.defer {
      if n == 0 then IO.unit
      else if n == pause then
        IO.println("paused...") *> waiter.get *> countdown(n - 1, pause, waiter)
      else countdown(n - 1, pause, waiter)
    }

  def run: IO[Unit] = for
    waiter <- IO.deferred[Unit]
    f <- countdown(10, 5, waiter).start
    _ <- IO.sleep(5.seconds)
    _ <- waiter.complete(())
    _ <- f.join
    _ <- IO.println("blast off!")
  yield ()

trait Latch:
  def release: IO[Unit]
  def await: IO[Unit]

enum State:
  case Awaiting(latches: Int, waiter: Deferred[IO, Unit])
  case Done

object Latch:
  import State._

  def apply(latches: Int): IO[Latch] = for
    waiter <- IO.deferred[Unit]
    state <- IO.ref[State](Awaiting(latches, waiter))
  yield new Latch {
    def release: IO[Unit] =
      state
        .modify {
          case Awaiting(n, waiter) =>
            if n > 1
            then (Awaiting(n - 1, waiter), IO.unit)
            else (Done, waiter.complete(()))
          case Done => (Done, IO.unit)
        }
        .flatten
        .void
    def await: IO[Unit] =
      state.get.flatMap {
        case Done                => IO.unit
        case Awaiting(_, waiter) => waiter.get
      }
  }

object Example8 extends IOApp.Simple:
  def run: IO[Unit] =
    IO.deferred[Unit].flatMap(s => IO(State.Awaiting(3, s)))

object Example9 extends IOApp.Simple:
  def run: IO[Unit] = for
    l <- Latch(10)
    _ <- (1 to 10).toList.traverse { i =>
      (IO.println(s"$i counting down") *> l.release).start
    }
    _ <- l.await
    _ <- IO.println("Got past the latch")
  yield ()

// https://typelevel.org/blog/2020/10/30/concurrency-in-ce3.html
object Exercises extends IOApp.Simple:
  def fibo(n: Long): Long = if n == 0 then 1 else n * fibo(n - 1)
  // I'm not sure how am I supposed to give back a fully polymorphic A
  // in case of a timeout
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[Unit] =
    IO.race(io, IO.sleep(duration)).void

  def parTraverse[A, B](as: List[A])(f: A => IO[B]): IO[List[B]] = ???

  trait Semaphore:
    def acquire: IO[Unit]
    def release: IO[Unit]

  object Semaphore:
    def apply(permits: Int): IO[Semaphore] = for
      waiter <- IO.deferred[Unit]
      r <- IO.ref[Int](permits)
    yield new Semaphore {
      // def acquire: IO[Unit] = for
      //   n <- r.get
      //   _ <- if n == 0
      //           then waiter.get
      //           else r.modify(n => (n-1, IO.unit))
      // yield ()
      def acquire: IO[Unit] = r.modify { n =>
        if n == 0 then (n - 1, waiter.get) else (n - 1, IO.unit)
      }
      // def release: IO[Unit] = for
      //   n <- r.get
      //   _ <- if n == 0
      //           then waiter.complete(()) >> r.modify(n => (n+1, IO.unit))
      //           else r.modify(n => (n+1, IO.unit))
      // yield ()
      def release: IO[Unit] = r.modify { n =>
        if n == permits then (n, IO.unit) else (n + 1, waiter.complete(()))
      }
    }

  // def run: IO[Unit] = timeout(IO(fibo(100)).flatMap(IO.println(_)), 5.seconds)
  def run: IO[Unit] = for
    s <- Semaphore(1)
    _ <- (s.acquire >> IO.println("thread 1 acq") >> IO.sleep(2.seconds) >> IO
      .println("thread 1 rel") >> s.release).start
    _ <- (s.acquire >> IO.println("thread 2 acq") >> IO.sleep(2.seconds) >> IO
      .println("thread 2 rel") >> s.release).start
    // _ <- s.acquire.start
    _ <- IO.println("waitin to acquire")
    _ <- s.acquire
    _ <- s.release
  yield ()