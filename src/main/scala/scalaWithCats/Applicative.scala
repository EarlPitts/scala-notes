package Applicative

object Motivation {
  import cats._
  import cats.implicits._

  def parseInt(str: String): Either[String, Int] =
    Either
      .catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => s"Couldn't read string")

  // We can't collect all errors, it returns after the first failure
  println(for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield a + b + c)
}

object SemigroupalExamples {
  import cats._
  import cats.implicits._

  // Only works for 2-tuples
  println(Semigroupal[Option].product(Some(1), Some(2)))
  println(Semigroupal[Option].product(Some(1), None))

  // Generalized versions for n-tuples
  println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))
  println(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))

  println(Applicative[Option].map2(Some(1), Some(2))(_ + _))
  println(Semigroupal.map2(Option(1), Option(2))(_ + _))

  println(
    (Some(1), Some(2)).sequence
  ) // Nope, the first projection is part of the context

  // Semigroupal uses the Monad instance for its implementation!
  println(Semigroupal[List].product(List(1,2), List(3,4)))
}

object ApplySyntax {
  import cats.instances.option._
  import cats.syntax.apply._
  import cats.syntax.semigroup._

  // Works automatically up to 22-tuples
  println((Option(123), Option("abc")).tupled)
  println((Option(123), Option("abc"), Option('e')).tupled)
  println((Option(123), Option(456), Option(789)).mapN(_ + _ + _))
  println((Option(123), Option.empty[Int], Option(789)).mapN(_ + _ + _))

  final case class Cat(name: String, born: Int, color: String)

  println(
    (
      Option("Garfield"),
      Option(1978),
      Option("Orange")
    ).mapN(Cat.apply)
  )
}

@main
def main: Unit =
  println("-" * 50)
  // Motivation
  // SemigroupalExamples
  ApplySyntax
  println("-" * 50)

import cats.effect._

object App extends IOApp.Simple {
  import cats._
  import cats.implicits._

  def run: IO[Unit] = {
    // Semigroupal[IO].product(IO.println(2), IO.println(3)).void
    IO.both(IO.println(2), IO.println(3)).void
  }

}
