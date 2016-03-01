import chapter10._
import SampleMonoids._

stringMonoid.op("howdy", "ho")

listMonoid.op(List(1, 2, 3), listMonoid.zero)

intAddition.op(5, 3)
intMultiplication.op(5, 3)

booleanOr.op(true, false)
booleanAnd.op(true, false)

optionMonoid.op(optionMonoid.zero, Some(3))

val succ = (a: Int) => a + 1
val double = (a: Int) => a + a
endoMonoid.op(succ, double)(10)
