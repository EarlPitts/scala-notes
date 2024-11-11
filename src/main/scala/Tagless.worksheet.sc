trait Exp[T]:
  def lit(i: Int): T
  def neg(t: T): T
  def add(l: T, r: T): T

object Exp:
  def apply[T](implicit e: Exp[T]) = e

  implicit val evaluator: Exp[Int] = new Exp[Int] {
    def lit(i: Int): Int = i
    def neg(t: Int): Int = -t
    def add(l: Int, r: Int): Int = l + r
  }

  implicit val pp: Exp[String] = new Exp[String] {
    def lit(i: Int): String = s"$i"
    def neg(t: String): String = s"(-$t)"
    def add(l: String, r: String): String = s"($l + $r)"
  }

object ExpSyntax:
  def lit[T: Exp](i: Int) = Exp[T].lit(i)
  def neg[T: Exp](t: T) = Exp[T].neg(t)
  def add[T: Exp](l: T, r: T) = Exp[T].add(l, r)

import ExpSyntax._

def expr1[T: Exp]: T = neg(add(lit(2), lit(3)))

expr1[Int]
expr1[String]
