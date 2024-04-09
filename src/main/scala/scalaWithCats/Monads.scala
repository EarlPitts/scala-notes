package Monads

// import cats.syntax.MonadOps
// import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A,B](ma: M[A])(f: A => M[B]): M[B]
  def map[A,B](ma: M[A])(f: A => B): M[B] = bind(ma)((a: A) => pure(f(a)))
}
  
object Monad {
  def apply[M[_]](implicit instance: Monad[M]): Monad[M] = instance

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def bind[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match
      case Some(a) => f(a)
      case None    => None
  }

  implicit val ListMonad: Monad[List] = new Monad[List] {
    def join[A](ls: List[List[A]]): List[A] = ls.foldRight(Nil)((l: List[A], acc: List[A]) => l ++ acc)
    def pure[A](a: A) = List(a)
    def bind[A,B](as: List[A])(f: A => List[B]): List[B] = as match
      case Nil => Nil
      case as => join(as.map(f))
  }
  
  // TODO Figure this out
  // type EitherA[A] = Either[A,_]
  // implicit def EitherMonad: Monad[EitherA] = new Monad[EitherA] {
  //   def pure[A](a: A) = Right(a)
  //   def bind[A,B](mab: EitherA[A])(f: B => Either[A,B]): Either[A,B] = mab match
  //     case Left(a) => Left(a)
  //     case Right(b) => f(b)
  // }
}

// object MonadSyntax {
//   implicit class MonadOps[M](value: M):
//     def >>=[A,B](f: A => M[B])(implicit instance: Monad[M]) =
//       instance.bind(value)(f)
//
// }


@main
def main: Unit =
  println(Monad[List].bind(List(1,2,3))(List(_,2)))
  println(Monad[List].map(List(1,2,3))(_ + 1))
  println(Monad[Option].map(Some(2))(_ + 2))
  println(Monad[Option].map(None)((x: Int) => x + 2))
