package textboard.domain

import scala.concurrent.duration.FiniteDuration
import cats.*
import cats.implicits.*

case class Post(id: Int, text: String, time: FiniteDuration)

object Post:
  given Show[Post] with
    def show(p: Post): String =
      s"\tPost no. ${p.id}\t${p.text}\t${p.time}\n"
