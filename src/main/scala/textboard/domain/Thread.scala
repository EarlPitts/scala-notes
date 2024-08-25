package textboard.domain

import cats.*
import cats.implicits.*

case class Thread(id: Int, posts: List[Post]) // extends AnyVal

object Thread:
  given Show[Thread] with
    def show(t: Thread): String =
      s"------------------------------------------------------\n" ++
      s"Thread no. ${t.id}\n${t.posts.map(_.show).mkString("\n")}"
