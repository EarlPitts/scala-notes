package Monads

// import cats.syntax.MonadOps
// import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A,B](ma: M[A])(f: A => M[B]): M[B]
  def map[A,B](ma: M[A])(f: A => B): M[B] = bind(ma)((a: A) => pure(f(a)))
}

object Monad {
  def apply[M[_]](implicit instance: Monad[M]): Monad[M] = instance

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def bind[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match
      case Some(a) => f(a)
      case None    => None
  }

  implicit val ListMonad: Monad[List] = new Monad[List] {
    def join[A](ls: List[List[A]]): List[A] = ls.foldRight(Nil)((l: List[A], acc: List[A]) => l ++ acc)
    def pure[A](a: A) = List(a)
    def bind[A,B](as: List[A])(f: A => List[B]): List[B] = as match
      case Nil => Nil
      case as => join(as.map(f))
  }

  // TODO Figure this out
  // type EitherA[A] = Either[A,_]
  // implicit def EitherMonad: Monad[EitherA] = new Monad[EitherA] {
  //   def pure[A](a: A) = Right(a)
  //   def bind[A,B](mab: EitherA[A])(f: B => Either[A,B]): Either[A,B] = mab match
  //     case Left(a) => Left(a)
  //     case Right(b) => f(b)
  // }
}

// object MonadSyntax {
//   implicit class MonadOps[M](value: M):
//     def >>=[A,B](f: A => M[B])(implicit instance: Monad[M]) =
//       instance.bind(value)(f)
//
// }

object Monading {
  import cats.Id
  import cats.implicits._

  def sumSquares[F[_]: cats.Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x + y

  println(Monad[List].bind(List(1,2,3))(List(_,2)))
  println(Monad[List].map(List(1,2,3))(_ + 1))
  println(Monad[Option].map(Some(2))(_ + 2))
  println(Monad[Option].map(None)((x: Int) => x + 2))
  println(Monading.sumSquares(Option(3), Option(4)))
  println(Monading.sumSquares(Id(2), Id(3)))
  println(CountPositives.countPositive(List(1,2,3,4)))
  println(CountPositives.countPositive(List(1,2,3,-1,4)))
  println(CountPositives.countPositive2(List(1,2,3,4)))
  println(CountPositives.countPositive2(List(1,2,3,-1,4)))

}

object SecretIdentity {
  type Id[A] = A

  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def bind[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }
}

object IdentityMonad {
  import SecretIdentity._
  import cats.Id

  println(Monad[Id].pure(3))
  println(Monad[Id].bind(3)(_ + 2))
}

object CountPositives {
  import cats._
  import cats.implicits._

  def countPositive(ns: List[Int]): Either[String, Int] =
    ns.foldLeft(0.asRight[String]) { (acc, n) =>
      if n < 0
      then Left("Negative, stopping")
      else acc.map(_ + 1)
    }

  def countPositive2(ns: List[Int]): Option[Int] =
    ns.foldLeft(Option(0)) { (acc, n) =>
      if n < 0
      then None
      else acc.map(_ + 1)
    }
}

object EitherStuff {
  import cats._
  import cats.implicits._
  import scala.util.Try

  // Using either to handle exceptions
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("bad")))

  // Converting to either
  println(Either.fromTry(Try(sys.error("bad"))))
  println(Either.fromOption(None, "bad"))

  // Transforming either
  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))
  println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))

  // Error handling
  println("Error".asLeft[Int].recover {
    case _ : String => -1
  })
  println("Error".asLeft[Int].recoverWith {
    case _ : String => Right(-1)
  })
}

object Errors {
  import scala.util.Try
  import cats._
  import cats.implicits._

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if age >= 18
    then me.pure(age)
    // then age.pure[F]
    else me.raiseError(IllegalArgumentException("Age must be over 18"))
    // else IllegalArgumentException("Age must be over 18").raiseError[F, Int]

  def validateAdult2[F[_]: MonadThrow](age: Int): F[Int] =
    if age >= 18
    then age.pure[F]
    else IllegalArgumentException("Age must be over 18").raiseError[F, Int]

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}

object EvalMonad {
  // Call-by-value
  val x = {
    println("ahoy")
    2
  }

  // Call-by-name
  def y = {
    println("ahoy")
    2
  }

  // Call-by-need
  lazy val z = {
    println("ahoy")
    2
  }

  import cats.Eval

  // You can use the `value` field to extract their value
  val now    = Eval.now(math.random + 1000)
  val always = Eval.always(math.random + 3000)
  val later  = Eval.later(math.random + 2000)

  def foldRight[A,B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        foldRight(tail, acc)(fn).map(fn(head, _))
      case Nil =>
        Eval.now(acc)
    }
}

object WriterMonad {
  import cats.data.Writer
  import cats._
  import cats.implicits._

  println(Writer(Vector(
    "One",
    "Two"
  ), 1859))

  type Logged[A] = Writer[Vector[String], A]

  // Needs a monoid instance in scope
  println(123.pure[Logged])

  def sajt: Writer[List[Int], Int] = for {
    _ <- List(1,2,3).tell
    _ <- List(3,4,5).tell
    c <- 2.writer(List(3,4,5))
  } yield c

  def sajt2: Writer[List[Int], Int] =
    List(1,2,3).tell.flatMap(_ =>
    List(3,4,5).tell.map(_ =>
    2))

  val writer1: Logged[Int] = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  // Transform the value in context
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  // Transform both
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  // Change value in context to identity value
  val writer5 = writer1.reset

  // Swap values
  val writer6 = writer1.swap

  println(writer1.run)
  println(writer1.written)
  println(writer1.value)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = for {
    ans <- if n == 0
           then 1.pure[Logged]
           else slowly(factorial(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

  // println(factorial(5).run)
  import scala.concurrent._
  import scala.concurrent.duration._

  val res = Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )).map(_.map(_.written)), 5.seconds)

  println(res)
}


@main
def main: Unit =
  // EitherStuff
  // Errors
  // EvalMonad
  // WriterMonad
