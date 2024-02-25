package Essentials

import cats.effect._

import cats.Order
import cats.implicits._, cats._, cats.derived._

import Color._
import Food._
import java.util.Date

enum Color:
  case Black, Ginger, TabbyWhite

enum Food:
  case Milk, Chips, Curry

case class Cat(val name: String, val color: Color, val food: Food)
object ChipShop:
  def willServe(cat: Cat): Boolean =
    cat match
      case Cat(_,_,Chips) => true
      case _              => false

case class Director(firstName: String, lastName: String, yearOfBirth: Int):
  def name: String = s"$firstName $lastName"

given Order[Director] with
  def compare(d1:Director, d2: Director): Int =
    if d1.yearOfBirth < d2.yearOfBirth
    then 1
    else if d1.yearOfBirth > d2.yearOfBirth then -1 else 0

object Director:
  def older(d1: Director, d2: Director): Director =
    Order[Director].max(d1,d2)

case class Film(
    name: String,
    yearOfRelease: Int,
    imdbRating: Double,
    director: Director
):
  def directorsAge: Int =
    director.yearOfBirth

  def isDirectedBy(d: Director): Boolean =
    director == d

object Film:
  def highestRating(f1: Film, f2: Film): Double =
    Order[Double].max(f1.imdbRating, f2.imdbRating)

  def oldestDirectorAtTheTime(f1: Film, f2: Film): Director =
    val n1 = f1.yearOfRelease - f1.directorsAge
    val n2 = f2.yearOfRelease - f2.directorsAge
    if n1 > n2 then f2.director else f1.director

case class Counter(val n: Int = 0):
  def succ(i: Int = 1): Counter = copy(n = n+i)
  def pred(i: Int = 1): Counter = copy(n = n-i)
  def adjust(a: Adder): Counter = copy(n = a(n))

class Adder(amount: Int):
  def apply(n: Int) = n + amount

case class Timestamp(seconds: Long)

object Timestamp:
  def apply(h: Int, m: Int, s: Int): Timestamp =
    Timestamp(h*60*60 + m*60 + s)

case class Person(firstName: String, lastName: String):
  def name = s"firstName lastName"
object Person:
  def apply(name: String): Option[Person] =
    name.split(" ") match
      case Array(firstName, lastName) => Some(Person(firstName, lastName))
      case _                          => None

object Dad:
  def rate(f: Film): Double =
    f match
      case Film(_,_,_,Director("Clint", "Eastwood", _)) => 10.0
      case Film(_,_,_,Director("John", "McTierman", _)) => 7.0
      case _                                            => 3.0
    
// enum Visitor:
//   case Anonymous(id: String, createdAt: Date = new Date())
//   case User(id: String, email: String, createdAt: Date = new Date())
//
// import Visitor._
//
// def checkEmail(v: Visitor): Option[String] =
//   v match
//     case Visitor.User(_,email,_) => Some(email)
//     case Visitor.Anonymous(_,_) => None

trait Visitor:
  def id: String
  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime

case class Anonymous(
  id: String,
  createdAt: Date = new Date()
) extends Visitor

case class User(
  id: String,
  email: String,
  createdAt: Date = new Date()
) extends Visitor


object App extends IOApp.Simple:

  def run: IO[Unit] = for
    _ <- IO.println("##########################")
    _ <- IO.println("##########################")
  yield ()
