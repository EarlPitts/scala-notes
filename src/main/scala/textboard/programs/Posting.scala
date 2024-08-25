package textboard.programs

import cats.effect.*
import cats.effect.kernel.*
import cats.effect.std.*
import cats.*
import cats.implicits.*

import textboard.services.*

def createThread[F[_]: Console: Monad](
    posts: Posts[F],
    threads: Threads[F]
): F[Int] = for
  text <- Console[F].readLine
  p <- posts.create(text)
  id <- threads.create(p)
yield id

def addPost[F[_]: Console: Monad](
    posts: Posts[F],
    threads: Threads[F],
    id: Int
): F[Unit] = for
  t <- threads.get(id)
  _ <- Monad[F].whenA(t.isDefined)(
    Console[F].readLine >>= (posts.create(_)) >>= (threads.add(_, id))
  )
yield ()

def showBoard[F[_]: Console: Monad](threads: Threads[F]): F[Unit] =
  threads.getAll >>= (_.traverse(Console[F].print).void)

