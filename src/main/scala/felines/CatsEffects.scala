package felines.CatsEffects

import cats.effect.{IO, Resource, IOApp, ExitCode}
import cats.effect.unsafe.implicits._
import scala.concurrent.duration._
import scala.io.Source
import cats.syntax.all._

import java.io._

object Main extends IOApp {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    val acquire = IO.blocking(new FileInputStream(f))
    val release =
      (inStream: FileInputStream) =>
        IO.blocking(inStream.close()) // .handleErrorWith(_ => IO.unit)
    Resource.make(acquire)(release)

// This is easier for types that implement AutoCloseable, but
// you cannot control what happens with exceptions
  def inputStreamAuto(f: File): Resource[IO, FileInputStream] =
    val acquire = IO(new FileInputStream(f))
    Resource.fromAutoCloseable(acquire)

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    val acquire = IO.blocking(new FileOutputStream(f))
    val release =
      (outStream: FileOutputStream) =>
        IO.blocking(outStream.close()) // .attempt.void
    Resource.make(acquire)(release)

  def inputOutputStreams(
      in: File,
      out: File
  ): Resource[IO, (InputStream, OutputStream)] =
    for {
      in <- inputStream(in)
      out <- outputStream(out)
    } yield (in, out)

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    def go(buffer: Array[Byte], acc: Long): IO[Long] =
      for {
        amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
        count <-
          if amount > -1
          then
            IO.blocking(destination.write(buffer, 0, amount)) >> go(
              buffer,
              acc + amount
            )
          else IO.pure(acc)
      } yield count
    go(new Array[Byte](1024 * 10), 0L)

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

  def copyBracket(origin: File, destination: File): IO[Long] =
    (IO(FileInputStream(origin)), IO(FileOutputStream(destination))).tupled
      .bracket { case (in, out) =>
        transfer(in, out)
      } { case (in, out) =>
        (IO(in.close()), IO(out.close())).tupled.void.handleErrorWith(_ =>
          IO.unit
        )
      }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <-
        if args.length < 2
        then
          IO.raiseError(
            new IllegalArgumentException("Need origin and destination files")
          )
        else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      count <- copy(orig, dest)
      _     <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}
