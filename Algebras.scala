// Semigroup
trait Semigroup[A]:
  def combine(a1: A, a2: A): A

def stringSemigroup: Semigroup[String] = new:
  def combine(a1: String, a2: String) = a1 + a2

def listSemigroup[A]: Semigroup[List[A]] = new:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2

// Monoid
trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

def stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String) = a1 + a2
  def empty = ""

def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  def empty = Nil

def intAddition: Monoid[Int] = new:
  def combine(a1: Int, a2: Int) = a1 + a2
  def empty = 0

def intMultiplication: Monoid[Int] = new:
  def combine(a1: Int, a2: Int) = a1 * a2
  def empty = 1

def booleanOr: Monoid[Boolean] = new:
  def combine(b1: Boolean, b2: Boolean) = b1 || b2
  def empty = false
  
def booleanAnd: Monoid[Boolean] = new:
  def combine(b1: Boolean, b2: Boolean) = b1 && b2
  def empty = true

def optionMonoid[A]: Monoid[Option[A]] = new:
  def combine(a1: Option[A], a2: Option[A]) =
    (a1, a2) match
      case (Some(a1), Some(a2)) => Some(a1)
      case (Some(a1), None)     => Some(a1)
      case (_,        Some(a2)) => Some(a2)
      case (_,_)                => None
  def empty = None

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(f: A => A, g: A => A) = f compose g
  def empty = identity

val _ = endoMonoid[Int].combine(_ + 1, _ + 2)(2) // 5
