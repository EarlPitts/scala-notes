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
      case Cat(_, _, Chips) => true
      case _                => false

case class Director(firstName: String, lastName: String, yearOfBirth: Int):
  def name: String = s"$firstName $lastName"

given Order[Director] with
  def compare(d1: Director, d2: Director): Int =
    if d1.yearOfBirth < d2.yearOfBirth
    then 1
    else if d1.yearOfBirth > d2.yearOfBirth then -1
    else 0

object Director:
  def older(d1: Director, d2: Director): Director =
    Order[Director].max(d1, d2)

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
  def succ(i: Int = 1): Counter = copy(n = n + i)
  def pred(i: Int = 1): Counter = copy(n = n - i)
  def adjust(a: Adder): Counter = copy(n = a(n))

class Adder(amount: Int):
  def apply(n: Int) = n + amount

case class Timestamp(seconds: Long)

object Timestamp:
  def apply(h: Int, m: Int, s: Int): Timestamp =
    Timestamp(h * 60 * 60 + m * 60 + s)

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
      case Film(_, _, _, Director("Clint", "Eastwood", _)) => 10.0
      case Film(_, _, _, Director("John", "McTierman", _)) => 7.0
      case _                                               => 3.0

object VisitorWithEnum:
  enum Visitor:
    case Anonymous(id: String, createdAt: Date = new Date())
    case User(id: String, email: String, createdAt: Date = new Date())

  import Visitor._

  def checkEmail(v: Visitor): Option[String] =
    v match
      case Visitor.User(_, email, _) => Some(email)
      case Visitor.Anonymous(_, _)   => None

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
    case User(_, email, _) => Some(email)
    case Anonymous(_, _)   => None

object Felines:
  enum Sound:
    case Meow, Roar

  sealed trait Feline:
    def color: Color
    def sound: Sound

  case class Tiger(color: Color, sound: Sound) extends Feline
  case class Lion(color: Color, sound: Sound, maneSize: Int) extends Feline
  case class Panther(color: Color, sound: Sound) extends Feline
  case class Cat(name: String, color: Color, food: Food, sound: Sound)
      extends Feline

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
    override val perimeter = 2 * width + 2 * height
    override val area = width * height

  case class Circle(radius: Double, color: Color) extends Shape:
    def sides: Int = Int.MaxValue
    def perimeter: Double = 2 * radius * math.Pi
    def area: Double = radius * radius * math.Pi

  case class Rectangle(height: Double, width: Double, color: Color)
      extends Rectangular

  case class Square(size: Double, color: Color) extends Rectangular:
    def height = size
    def width = size

  object Draw:
    def apply(c: Color): String =
      c match
        case Red    => "red"
        case Yellow => "yellow"
        case Pink   => "pink"
        case custom => if custom.isLight then "light" else "dark"

    def apply(s: Shape): String =
      s match
        case Circle(radius, color) =>
          s"A ${Draw(color)} circle of radius $radius"
        case Square(size, color) => s"A ${Draw(color)} square of size $size"
        case Rectangle(height, width, color) =>
          s"A ${Draw(color)} rectangle with height $height and width $width"

object Water:
  trait Source
  case object Well extends Source
  case object Spring extends Source
  case object Tap extends Source

  case class BottledWater(size: Int, source: Source, carbonated: Boolean)

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
        case Red    => Green
        case Green  => Yellow
        case Yellow => Red
  case object Red extends TrafficLight
  case object Green extends TrafficLight
  case object Yellow extends TrafficLight

  def next(t: TrafficLight): TrafficLight =
    t match
      case Red    => Green
      case Green  => Yellow
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
      case Success(n1) =>
        if n2 == 0 then Failure("Division by zero") else Success(n1 / n2)
      case fail => fail

object Laziness:
  case class LazyList[A](head: A, tail: () => LazyList[A])

  object LazyList:
    def repeat[A](a: A): LazyList[A] =
      LazyList(a, () => repeat(a))

// enum Maybe[+A]:
//   case Nothing
//   case Just(a: A)
//
//
//   def foldr[B](f: (A,B) => B)(z: B): B =
//     this match
//       case Nothing => z
//       case Just(a) => f(a,z)
//
//   def map[B](f: A => B): Maybe[B] =
//     flatMap[B](a => Just(f(a)))
//
//   def flatMap[B](f: A => Maybe[B]): Maybe[B] =
//     this match
//       case Just(a) => f(a)
//       case Nothing => Nothing
//
// import Maybe.*

sealed trait Maybe[+A]:

  def foldr[B](f: (A, B) => B)(z: B): B =
    this match
      case Nothing => z
      case Just(a) => f(a, z)

  def map[B](f: A => B): Maybe[B] =
    flatMap[B](a => Just(f(a)))

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    this match
      case Just(a) => f(a)
      case Nothing => Nothing
case class Just[A](a: A) extends Maybe[A]
case object Nothing extends Maybe[Nothing]

// sealed trait Functor[F[_]]:
//   def map[A,B](fa: F[A])(f: A => B): F[B]
//
//   def as[A,B](fa: F[A])(b: B): F[B] = map(fa)(_ => b)
//   def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

  def foldr[B](f: (A, B) => B)(z: B): B =
    this match
      case Nil        => z
      case Cons(h, t) => f(h, t.foldr(f)(z))

  def foldrTail[B](f: (A, B) => B)(z: B): B =
    def go(t: MyList[A], acc: B): B =
      t match
        case Nil        => acc
        case Cons(h, t) => go(t, f(h, acc))
    go(this, z)

  def map[B](f: A => B): MyList[B] =
    foldr((a: A, bs: MyList[B]) => Cons(f(a), bs))(Nil)

  def bind[B](f: A => MyList[B]): MyList[B] =
    MyList.join(map(f))

  def length: Int =
    this.foldr((_, len: Int) => len + 1)(0)

  def apply(n: Int): Maybe[A] =
    this match
      case Nil         => Nothing
      case Cons(a, as) => if n == 0 then Just(a) else as(n - 1)

  // Yes I know this looks terrible
  override def toString: String =
    foldr((a: A, s: String) => s"$a,$s")("")

  // def double[A : Numeric]: MyList[A] =
  //   this.foldr((x: A, l: MyList[A]) => Cons(x*2,l))(Nil)

object MyList:
  def apply[A](l: A*): MyList[A] =
    if l.isEmpty
    then Nil
    else Cons(l.head, apply(l.tail*))

  def ++[A](l1: MyList[A], l2: MyList[A]): MyList[A] =
    l1.foldr((a: A, l: MyList[A]) => Cons(a, l))(l2)

  def join[A](ls: MyList[MyList[A]]): MyList[A] =
    ls.foldr((l1: MyList[A], l2: MyList[A]) => MyList.++(l1, l2))(Nil)

  def sum[A: Monoid](l: MyList[A]): A =
    l.foldr((a: A, b: A) => Monoid[A].combine(a, b))(Monoid[A].empty)

  def contains[A](l: MyList[A], item: A): Boolean =
    l.foldr((a: A, res: Boolean) => a == item || res)(false)
    // l match
    //   case Nil => false
    //   case Cons(a,as) => if a == item then true else contains(as,item)

enum Tree[A]:
  case Leaf(a: A)
  case Node(l: Tree[A], r: Tree[A])

  def foldr[B](f: (A, B) => B)(z: B): B =
    this match
      case Leaf(a)    => f(a, z)
      case Node(l, r) => l.foldr(f)(r.foldr(f)(z))

object Tree:
  def sum[A: Monoid](l: Tree[A]): A =
    l.foldr((a: A, b: A) => Monoid[A].combine(a, b))(Monoid[A].empty)

given Functor[Maybe] with
  def map[A, B](m: Maybe[A])(f: A => B): Maybe[B] =
    m match
      case Nothing => Nothing
      case Just(a) => Just(f(a))

def id[A](a: A): A = a

case class Pair[A, B](a: A, b: B)

// sealed trait Either[A,B]
// case class Left[A,B](a: A) extends Either[A,B]
// case class Right[A,B](b: B) extends Either[A,B]

case class Box[+A](value: A):
  def set[AA >: A](a: AA): Box[AA] = Box(a)

sealed trait Sum[+A, +B]:
  def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
    this match
      case Failure(a) => Failure(a)
      case Success(b) => f(b)

case class Failure[A](a: A) extends Sum[A, Nothing]
case class Success[B](b: B) extends Sum[Nothing, B]

enum Either[A, +B]:
  case Left(a: A)
  case Right(b: B)

  def value: A | B =
    this match
      case Left(x)  => x
      case Right(x) => x

  def fold[C](f: A => C, g: B => C): C =
    this match
      case Left(a)  => f(a)
      case Right(b) => g(b)

  def map[C](f: B => C): Either[A, C] =
    this match
      case Left(a)  => Left(a)
      case Right(b) => Right(f(b))

  def flatMap[C](f: B => Either[A, C]): Either[A, C] =
    this match
      case Left(a)  => Left(a)
      case Right(b) => f(b)

import Either.*

def intOrString(g: Boolean): Either[Int, String] =
  if g then Left(123) else Right("abc")

object Flatmapping:
  List(Just(1), Just(2), Just(3)).map((ma: Maybe[Int]) =>
    ma.flatMap((n: Int) => if n % 2 == 0 then Just(n) else Nothing)
  )

object NewCalculator:
  type Res = Either[String, Double]

  enum Expression:
    case Addition(l: Expression, r: Expression)
    case Subtraction(l: Expression, r: Expression)
    case Division(l: Expression, r: Expression)
    case SquareRoot(e: Expression)
    case Number(n: Int)

    def div(l: Expression, r: Expression): Res = for
      n1 <- l.eval
      n2 <- r.eval
      res <- if n2 == 0 then Left("division by zero") else Right(n1 / n2)
    yield res
    // l.eval.flatMap(n1 => r.eval.flatMap(n2 => if n2 == 0 then Left("division by zero") else Right(n1 / n2)))

    def bin(l: Expression, r: Expression, op: (Double, Double) => Double): Res =
      for
        n1 <- l.eval
        n2 <- r.eval
      yield op(n1, n2)
    // l.eval.flatMap(n1 => r.eval.flatMap(n2 => Right(op(n1,n2))))

    def un(e: Expression, op: Double => Double): Res =
      e.eval.flatMap(n => Right(op(n)))

    def eval: Res =
      this match
        case Addition(l, r)    => bin(l, r, _ + _)
        case Subtraction(l, r) => bin(l, r, _ - _)
        case Division(l, r)    => div(l, r)
        case SquareRoot(e)     => un(e, math.sqrt)
        case Number(n)         => Right(n)

object Collections:
  // Sequences
  val seq = Seq(1, 2, 3)
  seq.apply(0) // 1
  seq(0) // 1
  seq(3) // Throws a runtime error
  seq.head
  seq.tail
  seq.headOption
  seq.length
  seq.size // same as length
  seq.contains(2) // true
  seq.find(_ == 2) // accepts a predicate, returns Option[A]
  seq.sortWith(_ > _) // descending order
  seq :+ 4 // append
  0 +: seq // prepend
  0 +: seq :+ 4
  seq ++ seq
  seq.mkString("[", ",", "]")

  // Lists
  val l = 1 :: 2 :: 3 :: Nil
  0 :: l
  List(1, 2, 3) ::: l // List concat

object FilmExercises:
  case class Film(name: String, yearOfRelease: Int, imdbRating: Double)
  case class Director(
      firstName: String,
      lastName: String,
      yearOfBirth: Int,
      films: Seq[Film]
  )
  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)
  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)
  val eastwood = new Director(
    "Clint",
    "Eastwood",
    1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus)
  )
  val mcTiernan = new Director(
    "John",
    "McTiernan",
    1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair)
  )
  val nolan = new Director(
    "Christopher",
    "Nolan",
    1970,
    Seq(memento, darkKnight, inception)
  )
  val someGuy = new Director("Just", "Some Guy", 1990, Seq())
  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def moreFilms(numOfFilms: Int): Seq[Director] =
    directors.filter(_.films.length > numOfFilms)

  def bornBefore(year: Int): Option[Director] =
    directors.find(_.yearOfBirth < year)

  def composed(numOfFilms: Int, year: Int): Seq[Director] =
    moreFilms(numOfFilms).filter(_.yearOfBirth < year)

  def sortThem(ascending: Boolean = true): Seq[Director] =
    if ascending
    then directors.sortWith((d1, d2) => d1.yearOfBirth < d2.yearOfBirth)
    else directors.sortWith((d1, d2) => d1.yearOfBirth > d2.yearOfBirth)

object App extends IOApp.Simple:

  import FilmExercises.*

  def stuff: List[Any] = List(
    sortThem(false)
  )

  def run: IO[Unit] = for
    _ <- IO.println("##########################")
    _ <- stuff.traverse(IO.println(_))
    _ <- IO.println("##########################")
  yield ()
