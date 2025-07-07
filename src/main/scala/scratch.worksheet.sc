import io.circe.*
import io.circe.parser.*
import io.circe.optics.JsonPath.*
import cats.effect.std.Dispatcher
import cats.effect._
import cats._
import cats.implicits._

enum Exp:
  case Lit(n: Int)
  case Neg(e: Exp)
  case Add(e1: Exp, e2: Exp)
import Exp.*

enum NumBool:
  case Bool(b: Boolean)
  case Num(n: Int)
  case Neg(e: Exp)
  case Add(e1: Exp, e2: Exp)
import Exp.*

def eval(e: Exp): Int = e match
  case Lit(n) => n
  case Add(e1, e2) => eval(e1) + eval(e2)
  case Neg(e) => - eval(e)

def pp(e: Exp): String = e match
  case Add(e1, e2) => s"(${pp(e1)} + ${pp(e2)})"
  case Lit(n) => s"$n"
  case Neg(e) => s"(-${pp(e)})"

def ppPrefix(e: Exp): String = e match
  case Add(e1, e2) => s"(+ ${pp(e1)} ${pp(e2)})"
  case Lit(n) => s"$n"
  case Neg(e) => s"(-${pp(e)})"

def evalNumBool(e: Exp): Int = e match
  case Lit(n) => n
  case Add(e1, e2) => eval(e1) + eval(e2)
  case Neg(e) => - eval(e)

val e = Add(Add(Lit(7), Lit(3)), Add(Lit(2), Neg(Lit(4))))
eval(e)
pp(e)
ppPrefix(e)


//-------------------
trait FinalExp[Repr]:
  def lit(n: Int): Repr
  def add(e1: Repr, e2: Repr): Repr
  def neg(e: Repr): Repr

object FinalExp:
  def eval: FinalExp[Int] = new FinalExp[Int] {
    def add(e1: Int, e2: Int): Int = e1 + e2
    def lit(n: Int): Int = n
    def neg(e: Int): Int = -e
  }

import Exp.*

object View extends FinalExp[String]:
  def add(e1: String, e2: String): String = s"(${e1} + ${e2})"
  def lit(n: Int): String = s"$n"
  def neg(e: String): String = s"-$e"

val x = FinalExp.eval

x.add(x.lit(2), x.lit(3))
View.add(View.lit(2), View.lit(3))

class Program[Repr](exp: FinalExp[Repr]):
  val repr = exp.add(exp.lit(2), exp.lit(3))

Program(x).repr
