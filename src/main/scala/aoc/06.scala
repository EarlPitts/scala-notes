package aoc

import cats.data.State
import cats.syntax.all._
import cats.effect._
import cats.Traverse
import cats.Traverse._

import java.lang.System.out

import scala.io.Source

type Memory = List[Int]
type Index = Int

def solve(m: Memory): State[List[Memory], Int] = for {
  seen <- State.get[List[Memory]]
  newMem = step(m)
  result <- if seen.contains(newMem)
              then State.pure(seen.length + 1)
              else State.set(newMem :: seen) >> solve(newMem)
} yield result

def step(m: Memory): Memory =
  val i = maxIndex(m)
  val n = m(i)
  val m2 = m.take(i) ++ List(0) ++ m.drop(i+1)
  redist(m2,(i+1),n)

def redist(m: Memory, i: Index, n: Int): Memory =
  n match
    case 0 => m
    case _ =>
      val lastBlock = m.length == i
      if lastBlock
        then redist(incBlock(m,0),1,(n-1))
        else redist(incBlock(m,i),(i+1),(n-1))

def maxIndex(m: Memory): Index = m.indexOf(m.max)

def incBlock(m: Memory, i: Index): Memory =
  val n = m(i)
  m.take(i) ++ List(n+1) ++ m.drop(i+1)

def solution(s: String): String =
  val mem = s.split("\\s+").toList.map(_.toInt)
  solve(mem).runA(List()).value.toString()

object Main extends IOApp.Simple:
  def run = for {
    f <- IO.blocking(Source.fromFile("input.txt"))
    _ <- IO.println(solution(f.getLines().toList.head))
  } yield ()
