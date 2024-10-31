class Animal()
class Dog() extends Animal

val a = Dog()

enum MyList[A]:
  case Cons(a: A, as: MyList[A])
  case Nil()

import MyList.*

Cons(Dog(), Nil()): MyList[Animal]

List[Animal](a)

import scala.reflect.runtime.universe.*

def isSubtype[A: TypeTag, B: TypeTag]: Boolean =
  typeOf[A] <:< typeOf[B]

def assertSubtype[A, B](implicit ev: A <:< B): Unit =
  // If this compiles, A is a subtype of B
  println(s"${implicitly[A <:< B]}: ${implicitly[A <:< B].getClass}")

// assertSubtype[MyList[Dog | ], MyList[Animal]]
// assertSubtype[Set[], Set[Animal]]
// assertSubtype[List[Dog], List[Animal]]
assertSubtype[Function1[Animal, Int], Function1[Dog, Int]]
// assertSubtype[Int, String]

// typeOf[Int]
//
// isSubtype[List[Dog], AnyVal]



// val s = Set(Dog,Animal()).size
