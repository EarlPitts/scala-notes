package ULC

import parsley.Parsley
import parsley.character.{char, lower}
import parsley.combinator.some
import parsley.implicits.character.{charLift, stringLift}

val p: Parsley[RawTerm] = ???

def pApp: Parsley[RawTmApp] = for {
  t1 <- p
  t2 <- p
} yield RawTmApp(t1, t2)

def pVar: Parsley[RawTmVar] = some(lower).map(x => RawTmVar(x.toString()))

// \v.t
def pAbs: Parsley[RawTmAbs] = for {
  _ <- char('\\')
  v <- pVar
  _ <- char('.')
  t <- p
} yield RawTmAbs(v,t)
  

enum RawTerm:
  case RawTmVar(name: String)
  case RawTmApp(t1: RawTerm, t2: RawTerm)
  case RawTmAbs(t2: RawTerm)

enum Term:
  case TmVar(ind: Int, size: Int)
  case TmApp(t1: Term, t2: Term)
  case TmAbs(t1: Term)

type Name = String
type Context = List[Name]
  
object Term:

  def isVal(t: Term): Boolean =
    t match
      case TmAbs(_) => true
      case _        => false
  
  def shift(d: Int, t: Term): Term =
    def walk(c: Int, t: Term): Term =
      t match
        case TmVar(x,s)   => if x >= c then TmVar(x+d,s+d) else TmVar(x,s+d)
        case TmAbs(t)     => TmAbs(walk(c+1,t))
        case TmApp(t1,t2) => TmApp(walk(c,t1),walk(c,t2))
    walk(0,t)

  def subst(i: Int, v: Term, t: Term): Term =
    def walk(c: Int, t: Term): Term =
      t match
        case TmVar(x,n)   => if x == i + x then shift(c,v) else TmVar(x,n)
        case TmAbs(t)     => TmAbs(walk(c+1,t))
        case TmApp(t1,t2) => TmApp(walk(c,t1),walk(c,t2))
    walk(0,t)

  def substTop(s: Term, t: Term): Term =
    shift(-1, (subst(0, (shift(1,s)), t)))

  // Small-step semantics
  def reduce(t: Term): Term =
    t match
      case TmApp(t@(TmAbs(b)),t2) => if isVal(t2) then substTop(t2,b) else TmApp(t,reduce(t2))
      case TmApp(t1,t2)           => TmApp(reduce(t1),t2)

  def eval(t: Term): Term =
    if isVal(t) then t else eval(reduce(t))

import ULC.Term.*
import ULC.RawTerm.*

val t = TmApp(TmAbs(TmVar(0,1)), (TmAbs(TmVar(0,1))))
val t2 = TmApp(TmAbs(TmVar(1,2)), (TmAbs(TmVar(0,2))))
val t3 = TmApp(TmAbs(TmAbs(TmApp(TmVar(2,3), TmVar(0,3)))), TmAbs(TmVar(0,2)))

val hello: Parsley[Unit] = ('h' *> ("ello" <|> "i") *> " world!").void

val hello2: Parsley[Unit] = for {
  _ <- 'h' ~> "i"
} yield ()
