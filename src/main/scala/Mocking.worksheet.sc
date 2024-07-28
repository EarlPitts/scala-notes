import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*

case class Config(url: String, token: String)

def saySomething(times: Int): ReaderT[IO, Config, List[String]] =
  ReaderT { c =>
    val say = s"Saying the ${c.url}"
    List.fill(times)(say).traverse(IO.println) >>
      List.fill(times)(say).pure
  }

def totallySendTheRequest(why: String): ReaderT[IO, Config, String] =
  ReaderT { c =>
    IO.pure(s"Sending the request to ${c.url}, because $why")
      .flatTap(IO.println)
  }

import cats.effect.std.Console

trait Logger[F[_]]:
  def log: String => F[Unit]

object Logger:
  def mkLogger[F[_]: Monad: Console] = new Logger[F]:
    def log: String => F[Unit] = s => Console[F].println(s)

def logger[M[_]: Console](log: String): M[Unit] = Console[M].println(log)

type ConfigIOReader[A] = ReaderT[IO, Config, A]


def main[M[_]: Monad](
    somethingSaying: Int => M[List[String]],
    requestSending: String => M[String],
    logger: Logger[M]
): M[List[String]] = for
  says <- somethingSaying(3)
  _ <- logger.log("said")
  sends <- requestSending("why not")
  _ <- logger.log("sent")
yield sends :: says

val c = Config("example.com", "12345")

import cats.effect.unsafe.implicits.global

main[ConfigIOReader](saySomething, totallySendTheRequest, Logger.mkLogger).run(c).unsafeRunSync()

type W[X] = Writer[Map[String, Int], X]

def mockSaying(times: Int): W[List[String]] = Writer(Map("sayer" -> 1), List.fill(times)("valami"))
def mockSending(why: String): W[String] = Writer(Map("sender" -> 1), why)
def mockLogger(log: String): W[Unit] = Map("log" -> 1).tell

val testLogger = new Logger[IO]:
  def log = _ => IO.unit

// main(mockSaying, mockSending, testLogger)
