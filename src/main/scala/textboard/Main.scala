package textboard

import cats.effect.*
import cats.effect.kernel.*
import cats.effect.std.*
import cats.*
import cats.implicits.*

import textboard.domain.*
import textboard.programs.*
import textboard.services.*

object App extends IOApp.Simple:
  def run: IO[Unit] = for
    db <- Ref[IO].of(List.empty[Thread])
    threadCnt <- Ref[IO].of(0)
    postCnt <- Ref[IO].of(0)

    threads = Threads.inMemThreads(db, threadCnt)
    posts = Posts.inMemPosts(postCnt)

    _ <- program(threads, posts)
  yield ()

  def program[F[_]: Monad: Console](threads: Threads[F], posts: Posts[F]) = for
    id <- createThread(posts, threads)
    _ <- addPost(posts, threads, id)
    _ <- addPost(posts, threads, id)
    id2 <- createThread(posts, threads)
    _ <- addPost(posts, threads, id2)
    _ <- showBoard(threads)
  yield ()
