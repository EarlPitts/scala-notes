import cats.Applicative

object AExpr:

  enum Term:
    case TmTrue
    case TmFalse
    case TmIf(t1:Term, t2:Term, t3:Term)
    case TmZero
    case TmPred(t1:Term)
    case TmSucc(t1:Term)
    case TmIsZero(t1:Term)

  def isNumeric(t:Term): Boolean =
    t match
      case Term.TmZero    => true
      case Term.TmSucc(t) => isNumeric(t)
      case _              => false

  def eval1(t: Term): Option[Term] =
    t match
      case Term.TmIf(Term.TmTrue,t2,_)        => Some(t2)
      case Term.TmIf(Term.TmFalse,_,t3)       => Some(t3)
      case Term.TmIf(t1,t2,t3)                =>
        Applicative[Option].ap3(Some(Term.TmIf.apply))(eval1(t1), Some(t2), Some(t3))
      case Term.TmIsZero(Term.TmZero)         => Some(Term.TmTrue)
      case Term.TmIsZero(Term.TmSucc(t:Term)) => if isNumeric(t) then Some(Term.TmFalse) else None
      case Term.TmIsZero(t)                   => eval1(t).map(Term.TmIsZero.apply)
      case Term.TmPred(Term.TmZero)           => Some(Term.TmZero)
      case Term.TmPred(Term.TmSucc(t:Term))   => if isNumeric(t) then Some(t) else None
      case Term.TmPred(t:Term)                => eval1(t).map(Term.TmPred.apply)
      case Term.TmSucc(t:Term)                => eval1(t).map(Term.TmSucc.apply)
      case _ => None

  def eval(t: Term): Option[Term] =
    eval1(t) match
      case Some(t) => eval(t)
      case None    => Some(t)

val _ = Applicative[Option].ap(Some((x:Integer) => x + 1))(Some(2))
// val _ = Applicative[Option].ap2(Some((x:Integer) => x + 1))(Some(2))

import AExpr.*
import AExpr.Term.*

val t = TmFalse
val t2 = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))


