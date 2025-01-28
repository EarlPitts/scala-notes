package modules

enum Value:
  private case BoolValue(value: Boolean)
  private case IntValue(value: Int)
  private case FloatValue(value: Float)
  private case SpringValue(value: String)

object Value:
  def mkInt(value: Int): Option[Value] =
    if value < 0 || value > 100 then None else Some(IntValue(value))


val x: Option[Value] = Value.mkInt(12)

// Internals cannot be used directly!
// val y: Value = Value.BoolValue(true)
//
// def f(v: Value) = v match {
//   case Value.BoolValue(value) => ???
// }
