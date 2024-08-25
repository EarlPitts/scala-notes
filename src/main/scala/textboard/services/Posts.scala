package textboard.services

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.kernel.*
import cats.effect.std.*

import textboard.domain.*

trait Posts[F[_]]:
  def get(id: Int, thread: Thread): F[Option[Post]]
  def create(text: String): F[Post]
  def getAll(thread: Thread): F[List[Post]]

object Posts:
  def inMemPosts[F[_]: Monad: Clock](
      counter: Ref[F, Int]
  ): Posts[F] = new Posts[F]:
    def get(id: Int, thread: Thread): F[Option[Post]] =
      Monad[F].pure(thread.posts.find(_.id === id))
    def create(text: String): F[Post] = for
      id <- counter.get
      time <- Clock[F].realTime
      post = Post(id, text, time)
      _ <- counter.update(_ + 1)
    yield post
    def getAll(thread: Thread): F[List[Post]] = Monad[F].pure(thread.posts)
