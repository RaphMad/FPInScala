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

Monoid.wordCount("A lot of repetition and")
Monoid.wordCount("A lot of repetition and state required for RemoveResourceFromUnitSaga and AddResourceToUnitSaga")
Monoid.wordCount(" ")
Monoid.wordCount("")
Monoid.wordCount("     ")
Monoid.wordCount("wort")
Monoid.wordCount("wort ")
Monoid.wordCount(" wort")

val M1: Monoid[Map[String, Int]] =  Monoid.mapMergeMonoid(intAddition)

val x1 = Map("o1" -> 1, "o2" -> 2)
val x2 = Map("o1" -> 5, "o3" -> 7)
val x3 = M1.op(x1, x2)

val M: Monoid[Map[String, Map[String, Int]]] =  Monoid.mapMergeMonoid(Monoid.mapMergeMonoid(intAddition))

val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
val m2 = Map("o1" -> Map("i2" -> 3))
val m3 = M.op(m1, m2)

Monoid.bag(Vector("a", "rose", "is", "a", "rose"))

val m = Monoid.productMonoid(intAddition, intAddition)
val p = SampleFoldables.foldableList.foldMap(List(1,2,3,4))(a => (1, a))(m)
val mean = p._1 / p._2.toDouble