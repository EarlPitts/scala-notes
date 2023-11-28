// Comment
/* Another
 * comment */
/** Docstring */

// This object stuff can be omitted, Scala suuports top-level defs
// Just think of this as a module
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  // Return types can be omitted
  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

// For nullary procedures, we write parens, but not for functions
// This annotation signals that this is the entry point
  @main def printAbs(): Unit =
    println(formatAbs(-42))

// Pattern matching
def sajt(n: Int) =
  n match
    case 0 => "zero"
    case 1 => "one"
    case 2 => "two"
    case _ => "other"

def fact(n: Int): Int =
  n match
    case 0 => 1
    case n => n * fact(n-1)

def factTail(n: Int): Int =
  def go(n: Int, acc: Int): Int =
      n match
        case 0 => acc
        case _ => go(n-1, acc * n)
  go(n,1)
