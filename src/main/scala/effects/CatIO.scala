package effects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import scala.concurrent.duration._
import scala.concurrent._
import scala.io.Source
import cats.syntax.all._
import cats.effect.kernel.Deferred

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

object TickingClock extends IOApp.Simple:
  def run: IO[Unit] = tickingClock

  def tickingClock: IO[Unit] =
    IO.println(System.currentTimeMillis)
      >> IO.sleep(1.second)
      >> tickingClock

object Future1 extends App:
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  // val hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))
  // val world = Future(println(s"[${Thread.currentThread.getName}] World"))
  // val -> def delays execution
  def hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))
  def world = Future(println(s"[${Thread.currentThread.getName}] World"))

  // val hw1: Future[Unit] = for
  //   _ <- hello
  //   _ <- world
  // yield ()

  val hw1: Future[Unit] = hello.flatMap(_ => world.flatMap(_ => Future(())))

  Await.ready(hw1, 1.seconds) // This timeouts for some reason

  val hw2: Future[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 1.seconds)

object IOComp extends App:
  val hello = IO(println(s"[${Thread.currentThread.getName}] Hello"))
  val world = IO(println(s"[${Thread.currentThread.getName}] World"))

  // val hw1: IO[Unit] = for
  //   _ <- hello
  //   _ <- world
  // yield ()

  // val hw1: IO[Unit] = (hello >> world).void // This hangs, no idea why

  val hw2: IO[Unit] =
    (hello, world).parMapN((_, _) => ())

  // hw1.unsafeRunSync()
  hw2.unsafeRunSync()

object Example1 extends IOApp.Simple:
  def repeat(letter: String): IO[Unit] =
    IO.print(letter).replicateA(100).void

  def run: IO[Unit] = for
    fa <- (repeat("A") *> repeat("B")).as("foo!").start
    fb <- (repeat("C") *> repeat("D")).as("bar!").start
    ra <- fa.joinWithNever
    rb <- fb.joinWithNever
    _ <- IO.println(s"\ndone: a says: $ra, b says: $rb")
  yield ()

object Example2 extends IOApp.Simple:
  def run: IO[Unit] = for
    fiber <- (IO.println("hello!") >> IO.sleep(1.seconds)).foreverM.start
    _ <- IO.sleep(5.seconds)
    _ <- fiber.cancel
  yield ()

object Example3 extends IOApp.Simple:
  val a = IO.println("hello!").foreverM
  val b = IO.println("world!").replicateA_(10)
  // b should always win, as a runs forever

  def run: IO[Unit] = for
    c <- IO.race(a, b)
    _ <- (c match
      case Left(_)  => IO.println("a won")
      case Right(_) => IO.println("b won")
    )
  yield ()

object Example4 extends IOApp.Simple:
  def fact(n: Long): Long = if (n == 0) then 1 else n * fact(n - 1)

  def run: IO[Unit] = for
    res <- IO.race(IO(fact(20)), IO(fact(20)))
    _ <- res.fold(
      a => IO.println(s"Right hand side won: $a"),
      b => IO.println(s"Left hand side won: $b")
    )
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
      else if n == pause then IO.println("paused...") *> waiter.get *> countdown(n-1, pause, waiter)
      else countdown(n-1, pause, waiter)
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
  case Awaiting(latches: Int, waiter: Deferred[IO,Unit])
  case Done

object Latch:
  import State._

  def apply(latches: Int): IO[Latch] = for
    waiter <- IO.deferred[Unit]
    state <- IO.ref[State](Awaiting(latches, waiter))
  yield new Latch {
    def release: IO[Unit] =
      state.modify {
        case Awaiting(n, waiter) =>
          if n > 1
            then (Awaiting(n-1, waiter), IO.unit)
          else (Done, waiter.complete(()))
        case Done => (Done, IO.unit)
      }.flatten.void
    def await: IO[Unit] =
      state.get.flatMap {
        case Done => IO.unit
        case Awaiting(_, waiter) => waiter.get
      }
  }

object Example8 extends IOApp.Simple:
  def run: IO[Unit] =
    IO.deferred[Unit].flatMap(s => IO(State.Awaiting(3,s)))

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
  def fibo(n: Long): Long = if n == 0 then 1 else n * fibo(n-1)
  // I'm not sure how am I supposed to give back a fully polymorphic A
  // in case of a timeout
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[Unit] =
    IO.race(io, IO.sleep(duration)).void

  def parTraverse[A,B](as: List[A])(f: A => IO[B]): IO[List[B]] = ???

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
        if n == 0 then (n-1, waiter.get) else (n-1, IO.unit)
      }
      // def release: IO[Unit] = for
      //   n <- r.get
      //   _ <- if n == 0
      //           then waiter.complete(()) >> r.modify(n => (n+1, IO.unit))
      //           else r.modify(n => (n+1, IO.unit))
      // yield ()
      def release: IO[Unit] = r.modify { n =>
        if n == permits then (n, IO.unit) else (n+1, waiter.complete(()))
      }
    }


  // def run: IO[Unit] = timeout(IO(fibo(100)).flatMap(IO.println(_)), 5.seconds)
  def run: IO[Unit] = for
    s <- Semaphore(1)
    _ <- (s.acquire >> IO.println("thread 1 acq") >> IO.sleep(2.seconds) >> IO.println("thread 1 rel") >> s.release).start
    _ <- (s.acquire >> IO.println("thread 2 acq") >> IO.sleep(2.seconds) >> IO.println("thread 2 rel") >> s.release).start
    // _ <- s.acquire.start
    _ <- IO.println("waitin to acquire")
    _ <- s.acquire
    _ <- s.release
  yield ()
