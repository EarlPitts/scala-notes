import cats.effect.*
import cats.*
import cats.implicits.*

case class GreetingsHandle(
    sayHiPurely: String => String,
    sayHiImpurely: String => IO[String]
)

val handle1 = GreetingsHandle(
  name => s"Hello $name!",
  name => s"Hello $name!".pure
)

handle1.sayHiPurely("jani")
handle1.sayHiImpurely("jani")
