package Essentials

import cats.effect._

import cats.Order
import cats.implicits._, cats._, cats.derived._
import math.Numeric.Implicits.infixNumericOps

import Color._
import Food._
import java.util.Date

import scala.annotation.tailrec

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
    
object VisitorWithEnum:
  enum Visitor:
    case Anonymous(id: String, createdAt: Date = new Date())
    case User(id: String, email: String, createdAt: Date = new Date())

  import Visitor._

  def checkEmail(v: Visitor): Option[String] =
    v match
      case Visitor.User(_,email,_) => Some(email)
      case Visitor.Anonymous(_,_) => None

// Sealed traits can only be extended in the same file
// Can be used to implement sum types
// The compiler will complain for non-exhaustive pattern-matches
sealed trait Visitor:
  val id: String
  val createdAt: Date

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

def checkEmail(v: Visitor): Option[String] =
  v match
    case User(_,email,_) => Some(email)
    case Anonymous(_,_) => None

object Felines:
  enum Sound:
    case Meow, Roar

  sealed trait Feline:
    def color: Color
    def sound: Sound

  case class Tiger(color: Color, sound: Sound) extends Feline
  case class Lion(color: Color, sound: Sound, maneSize: Int) extends Feline
  case class Panther(color: Color, sound: Sound) extends Feline
  case class Cat(name: String, color: Color, food: Food, sound: Sound) extends Feline

object BetterFelines:
  enum Sound:
    case Meow, Roar

  enum Feline:
    case Tiger(color: Color, sound: Sound) 
    case Lion(color: Color, sound: Sound, maneSize: Int) 
    case Panther(color: Color, sound: Sound) 
    case Cat(name: String, color: Color, food: Food, sound: Sound) 

object Shapes:
  sealed trait Color:
    def red: Int
    def green: Int
    def blue: Int

    def isLight = red + green + blue > 3 * 255 / 2
  case object Red extends Color:
    val red = 255
    val green = 0
    val blue = 0
  case object Yellow extends Color:
    val red = 255
    val green = 255
    val blue = 0
  case object Pink extends Color:
    val red = 255
    val green = 0
    val blue = 255
  case class Custom(red: Int, green: Int, blue: Int) extends Color

  sealed trait Shape:
    def sides: Int
    def perimeter: Double
    def area: Double
    def color: Color

  sealed trait Rectangular extends Shape:
    def width: Double
    def height: Double
    val sides = 4
    override val perimeter = 2*width + 2*height
    override val area = width*height

  case class Circle(radius: Double, color: Color) extends Shape:
    def sides: Int = Int.MaxValue
    def perimeter: Double = 2 * radius * math.Pi
    def area: Double = radius * radius * math.Pi

  case class Rectangle(height: Double, width: Double, color: Color) extends Rectangular

  case class Square(size: Double, color: Color) extends Rectangular:
    def height = size
    def width = size

  object Draw:
    def apply(c: Color): String =
      c match
        case Red => "red"
        case Yellow => "yellow"
        case Pink => "pink"
        case custom => if custom.isLight then "light" else "dark"

    def apply(s: Shape): String =
      s match
        case Circle(radius, color) => s"A ${Draw(color)} circle of radius $radius"
        case Square(size, color) => s"A ${Draw(color)} square of size $size"
        case Rectangle(height, width, color) => s"A ${Draw(color)} rectangle with height $height and width $width"
    
object Water:
  trait Source
  case object Well extends Source
  case object Spring extends Source
  case object Tap extends Source
    
  case class BottledWater(
    size: Int,
    source: Source,
    carbonated: Boolean)

object TrafficExercise:

  // sealed trait TrafficLight:
  //   def next: TrafficLight
  // case object Red extends TrafficLight:
  //   def next: TrafficLight = Green
  // case object Green extends TrafficLight:
  //   def next: TrafficLight = Yellow
  // case object Yellow extends TrafficLight:
  //   def next: TrafficLight = Red

  sealed trait TrafficLight:
    def next: TrafficLight =
      this match
        case Red => Green
        case Green => Yellow
        case Yellow => Red
  case object Red extends TrafficLight
  case object Green extends TrafficLight
  case object Yellow extends TrafficLight

  def next(t: TrafficLight): TrafficLight =
    t match
      case Red => Green
      case Green => Yellow
      case Yellow => Red

object Calculator:
  // enum Result:
  //   case Success(n: Int)
  //   case Fail(reason: String)

  sealed trait Calculation
  case class Success(result: Int) extends Calculation
  case class Failure(reason: String) extends Calculation
  
  def +(c: Calculation, n2: Int): Calculation =
    c match
      case Success(n1) => Success(n1 + n2)
      case fail        => fail

  def -(c: Calculation, n2: Int): Calculation =
    c match
      case Success(n1) => Success(n1 - n2)
      case fail        => fail

  def /(c: Calculation, n2: Int): Calculation =
    c match
      case Success(n1) => if n2 == 0 then Failure("Division by zero") else Success(n1 / n2)
      case fail        => fail

object Laziness:
  case class LazyList[A](head: A, tail: () => LazyList[A])

  object LazyList:
    def repeat[A](a: A): LazyList[A] =
      LazyList(a, () => repeat(a))
  

enum Maybe[+A]:
  case Nothing
  case Just(a: A)

import Maybe.*

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

  def foldr[B](f: (A,B) => B)(z: B): B =
    this match
      case Nil => z
      case Cons(h,t) => f(h,t.foldr(f)(z))

  def foldrTail[B](f: (A,B) => B)(z: B): B =
    def go(t: MyList[A], acc: B): B =
      t match
        case Nil => acc
        case Cons(h,t) => go(t,f(h,acc))
    go(this, z)

  def length: Int =
    this.foldr((_, len: Int) => len + 1)(0)

  def apply(n: Int): Maybe[A] =
    this match
      case Nil        => Nothing
      case Cons(a,as) => if n == 0 then Just(a) else as(n-1)

object MyList:
  def apply[A](l: A*): MyList[A] =
    if l.isEmpty
    then Nil
    else Cons(l.head, apply(l.tail*))

  def sum[A: Monoid](l: MyList[A]): A =
    l.foldr((a: A, b: A) => Monoid[A].combine(a,b))(Monoid[A].empty)

  def contains[A](l: MyList[A], item: A): Boolean =
    l.foldr((a: A, res: Boolean) => a == item || res)(false)
    // l match
    //   case Nil => false
    //   case Cons(a,as) => if a == item then true else contains(as,item)


enum Tree[A]:
  case Leaf(a: A)
  case Node(l: Tree[A], r: Tree[A])

  def foldr[B](f: (A,B) => B)(z: B): B =
    this match
      case Leaf(a) => f(a,z)
      case Node(l,r) => l.foldr(f)(r.foldr(f)(z))

object Tree:
  def sum[A: Monoid](l: Tree[A]): A =
    l.foldr((a: A, b: A) => Monoid[A].combine(a,b))(Monoid[A].empty)

given Functor[Maybe] with
  def map[A,B](m: Maybe[A])(f: A => B): Maybe[B] =
    m match
      case Nothing => Nothing
      case Just(a) => Just(f(a))

def id[A](a: A): A = a

object App extends IOApp.Simple:
  import Tree.*

  def run: IO[Unit] = for
    _ <- IO.println("##########################")
    _ <- IO.println(MyList(1,2,3).foldrTail((a: Int, b: Int) => a + b)(0))
    _ <- IO.println(Functor[Maybe].map(Just(2))(_ + 1))
    _ <- IO.println(MyList.sum(MyList("a","b")))
    _ <- IO.println(MyList.contains(MyList(1,2,3,4),5))
    _ <- IO.println(Tree.sum(Node(Node(Leaf(2),Leaf(4)),Node(Node(Leaf(3),Leaf(5)),Leaf(8)))))
    _ <- IO.println(MyList(1,2,3,4)(3))
    _ <- IO.println("##########################")
  yield ()
