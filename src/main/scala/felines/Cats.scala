package felines.Cats

import cats.*
import cats.Applicative
import cats.data.*
import cats.syntax.all.*

def test(oa: Option[Int], ob: Option[Int]): Option[Int] =
  for
    a <- oa
    b <- ob
    c = a + b
  yield c

// def liftA2[F[_],A,B](f: A => B)(fa: F[A]): F[B] =
//   Applicative.pure(f).map(fa)

val _ = Applicative[Option].pure((x: Int) => (x + 1)) <*> Some(2)
val a = Applicative[Option].map2(Some(2),Some(3))((x: Int, y: Int) => (x + y)) // This is like LiftA2

val b = Applicative[Option].product(Some(2), Some(3))
val c = Applicative[List].product(List(1,2,3),List(4,5,6))
val d = Applicative[Option].ap2(Some((x:Int, y: Int) => (x + y)))(Some(2), Some(3))
val e = Some(2).map((x: Int) => (y: Int) => x + y) <*> Some(3)

val f = Some(2).flatMap(x => Some(x + 2))
