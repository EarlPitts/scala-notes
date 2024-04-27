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

object Semigroupal {
  import cats._
  import cats.implicits._
}

@main
def main: Unit =
  println("-" * 50)
  Motivation
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
